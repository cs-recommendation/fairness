import numpy as np
import argparse
from pathlib import Path

import pandas as pd
import torch
import torch.nn as nn
import torch.optim as optim
from tqdm import tqdm

from dataset import (
    preprocess_adult_data,
    preprocess_interpolation_data,
    preprocess_synthetic_data,
    preprocess_synthetic2_data,
)
from model import Net
from utils import train_dp, evaluate_dp
from utils import train_eo, evaluate_eo
from fliprate import preprocess_high_impact_samples
from adversarial import AdversarialDebiasingTorch
from reweight import fit_reweight_predictor
from fairshift import align_train_with_target


def run_experiments(
    method="mixup",
    mode="dp",
    lam=0.5,
    num_exp=10,
    dataset="adult",
    synthetic2_test_k=1,
    pre_process="",
):
    """
    Retrain each model for 10 times and report the mean ap and dp.

    Args:
        method: 训练方法 ('mixup', 'GapReg', 'erm', 'fliprate', 'fliprate-fairshift', 'adversarial', 'reweight', 'reweight-gapreg', 'reweight-fairmixup')
        mode: 公平性模式 ('dp', 'eo')
        lam: 正则化参数
        num_exp: 实验次数
        dataset: 数据集选择 ('adult', 'interpolation', 'synthetic', 'synthetic2')
        pre_process: 预处理方法 (''/ 'fairshift')，会在训练方法前对数据进行处理
    """

    # 如果使用fliprate方法，需要预处理高影响样本
    use_high_impact = method in {"fliprate", "fliprate-fairshift"}
    apply_fairshift = method == "fliprate-fairshift"
    high_impact_ratio = 0.2  # 默认比例

    if use_high_impact:
        print("Using fliprate method with high impact samples...")
        print("Preprocessing high impact samples for all seeds...")
        for i in range(num_exp):
            preprocess_high_impact_samples(
                seed=i,
                top_k=1000,
                force_recompute=apply_fairshift,
                dataset=dataset,
                mode=mode,
                synthetic2_test_k=synthetic2_test_k,
                apply_fairshift=apply_fairshift,
                high_impact_strategy="fairshift" if apply_fairshift else "default",
            )

    ap = []
    gap = []

    mf_results = [] if mode == "eo" else None

    for i in range(num_exp):
        print("On experiment", i)
        # get train/test data
        if dataset == "adult":
            X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
                preprocess_adult_data(seed=i)
            )
        elif dataset == "interpolation":
            X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
                preprocess_interpolation_data(seed=i)
            )
        elif dataset == "synthetic":
            X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
                preprocess_synthetic_data(seed=i)
            )
        elif dataset == "synthetic2":
            X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
                preprocess_synthetic2_data(seed=i, test_k=synthetic2_test_k)
            )
        else:
            raise ValueError(f"Unsupported dataset: {dataset}")

        if pre_process == "fairshift":
            print("Applying fairshift preprocessing before method...")
            (
                X_train,
                y_train,
                A_train,
            ) = align_train_with_target(
                X_train,
                y_train,
                A_train,
                y_test,
                A_test,
                random_state=i,
            )

        if method == "adversarial":
            adv_trainer = AdversarialDebiasingTorch(
                adversary_loss_weight=lam,
                random_state=i,
                debias=True,
                num_epochs=10,
                batch_size=256,
                verbose=False,
            )
            adv_trainer.fit(X_train, y_train, A_train)
            predictor = adv_trainer.get_predictor()
            predictor.eval()

            if mode == "dp":
                ap_val, gap_val = evaluate_dp(predictor, X_val, y_val, A_val)
                ap_test, gap_test = evaluate_dp(predictor, X_test, y_test, A_test)
            elif mode == "eo":
                ap_val, gap_val, mf_val = evaluate_eo(predictor, X_val, y_val, A_val)
                ap_test, gap_test, mf_test = evaluate_eo(
                    predictor, X_test, y_test, A_test
                )
            else:
                raise ValueError(f"Unsupported mode: {mode}")

            gap.append(gap_test)
            ap.append(ap_test)
            if mode == "eo":
                mf_results.append(mf_test)
            continue

        if method == "reweight":
            predictor = fit_reweight_predictor(
                X_train,
                y_train,
                A_train,
                X_val,
                y_val,
                A_val,
                mode=mode,
                lam=lam,
                random_state=i,
            )

            if mode == "dp":
                ap_val, gap_val = evaluate_dp(predictor, X_val, y_val, A_val)
                ap_test, gap_test = evaluate_dp(predictor, X_test, y_test, A_test)
            elif mode == "eo":
                ap_val, gap_val, mf_val = evaluate_eo(predictor, X_val, y_val, A_val)
                ap_test, gap_test, mf_test = evaluate_eo(
                    predictor, X_test, y_test, A_test
                )
            else:
                raise ValueError(f"Unsupported mode: {mode}")

            gap.append(gap_test)
            ap.append(ap_test)
            if mode == "eo":
                mf_results.append(mf_test)
            continue

        if pre_process != "fairshift" and method in {
            "reweight-gapreg",
            "reweight-fairmixup",
            "fliprate-fairshift",
        }:
            X_train, y_train, A_train = align_train_with_target(
                X_train,
                y_train,
                A_train,
                y_test,
                A_test,
                random_state=i,
            )

        # initialize model
        model = Net(input_size=len(X_train[0])).cuda()
        optimizer = optim.Adam(model.parameters(), lr=1e-3)
        criterion = nn.BCELoss()

        # run experiments
        ap_val_epoch = []
        gap_val_epoch = []
        ap_test_epoch = []
        gap_test_epoch = []
        mf_test_epoch = [] if mode == "eo" else None
        for j in tqdm(range(10)):
            # 如果是fliprate方法，实际使用mixup训练但加载高影响样本
            train_method = (
                "mixup"
                if method in {"fliprate", "fliprate-fairshift", "reweight-fairmixup"}
                else method
            )

            if mode == "dp":
                train_dp(
                    model,
                    criterion,
                    optimizer,
                    X_train,
                    A_train,
                    y_train,
                    train_method,
                    lam,
                    use_high_impact=use_high_impact,
                    high_impact_ratio=high_impact_ratio,
                    seed=i,
                    dataset=dataset,
                    mode=mode,
                    synthetic2_test_k=synthetic2_test_k,
                    high_impact_strategy="fairshift" if apply_fairshift else "default",
                )
                ap_val, gap_val = evaluate_dp(model, X_val, y_val, A_val)
                ap_test, gap_test = evaluate_dp(model, X_test, y_test, A_test)
            elif mode == "eo":
                train_eo(
                    model,
                    criterion,
                    optimizer,
                    X_train,
                    A_train,
                    y_train,
                    train_method,
                    lam,
                    use_high_impact=use_high_impact,
                    high_impact_ratio=high_impact_ratio,
                    seed=i,
                    dataset=dataset,
                    mode=mode,
                    synthetic2_test_k=synthetic2_test_k,
                    high_impact_strategy="fairshift" if apply_fairshift else "default",
                )
                ap_val, gap_val, mf_val = evaluate_eo(model, X_val, y_val, A_val)
                ap_test, gap_test, mf_test = evaluate_eo(model, X_test, y_test, A_test)

            if j > 0:  # 跳过第一个epoch的结果
                ap_val_epoch.append(ap_val)
                ap_test_epoch.append(ap_test)
                gap_val_epoch.append(gap_val)
                gap_test_epoch.append(gap_test)
                if mode == "eo":
                    mf_test_epoch.append(mf_test)

        # best model based on validation performance
        idx = gap_val_epoch.index(min(gap_val_epoch))
        gap.append(gap_test_epoch[idx])
        ap.append(ap_test_epoch[idx])
        if mode == "eo" and mf_test_epoch:
            mf_results.append(mf_test_epoch[idx])

    ap_mean = float(np.mean(ap)) if ap else float("nan")
    gap_mean = float(np.mean(gap)) if gap else float("nan")
    ap_std = float(np.std(ap)) if ap else float("nan")
    gap_std = float(np.std(gap)) if gap else float("nan")

    print("--------AVG---------")
    print("Average Precision", ap_mean)
    print(mode + " gap", gap_mean)

    mf_group_avg_json = None
    if mode == "eo" and mf_results:
        mf_group_frames = [mf.by_group for mf in mf_results if hasattr(mf, "by_group")]
        if mf_group_frames:
            mf_group_concat = pd.concat(mf_group_frames)
            mf_group_avg = mf_group_concat.groupby(level=0).mean()
            print(mode + " mf (avg)")
            print(mf_group_avg)
            mf_group_avg_json = mf_group_avg.to_json()

    result_summary = {
        "method": method,
        "mode": mode,
        "lam": lam,
        "num_exp": num_exp,
        "dataset": dataset,
        "synthetic2_test_k": synthetic2_test_k,
        "pre_process": pre_process,
        "ap_mean": ap_mean,
        "gap_mean": gap_mean,
        "ap_std": ap_std,
        "gap_std": gap_std,
    }
    if mf_group_avg_json is not None:
        result_summary["mf_by_group_mean"] = mf_group_avg_json

    return result_summary


def run_full_sweep(
    mode="dp",
    lam=0.5,
    num_exp=10,
    methods=None,
    synthetic2_test_k_list=None,
    pre_process_list=None,
    output_csv=None,
):
    """批量运行所有方法与测试集组合，并保存结果。"""

    if methods is None:
        methods = ["erm", "mixup", "GapReg", "fliprate", "adversarial", "reweight"]
    if synthetic2_test_k_list is None:
        synthetic2_test_k_list = [0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4]
    if pre_process_list is None:
        pre_process_list = ["", "fairshift"]

    results = []
    for pre_process in pre_process_list:
        for k in synthetic2_test_k_list:
            for method in methods:
                display_preprocess = pre_process or "none"
                print(
                    f"开始运行 method={method}, pre_process={display_preprocess}, synthetic2_test_k={k}"
                )
                summary = run_experiments(
                    method=method,
                    mode=mode,
                    lam=lam,
                    num_exp=num_exp,
                    dataset="synthetic2",
                    synthetic2_test_k=k,
                    pre_process=pre_process,
                )
                row = {
                    "method": method,
                    "mode": mode,
                    "lam": lam,
                    "num_exp": num_exp,
                    "dataset": "synthetic2",
                    "synthetic2_test_k": k,
                    "pre_process": display_preprocess,
                    "ap_mean": summary.get("ap_mean"),
                    "gap_mean": summary.get("gap_mean"),
                    "ap_std": summary.get("ap_std"),
                    "gap_std": summary.get("gap_std"),
                }
                if "mf_by_group_mean" in summary:
                    row["mf_by_group_mean"] = summary["mf_by_group_mean"]
                results.append(row)

    df = pd.DataFrame(results)
    sort_cols = ["pre_process", "synthetic2_test_k", "method"]
    df.sort_values(sort_cols, inplace=True)

    if output_csv is None:
        output_csv = Path("adult") / f"synthetic2_{mode}_summary.csv"
    output_path = Path(output_csv)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(output_path, index=False)

    print("汇总结果：")
    print(df.to_string(index=False))
    print(f"结果已保存到 {output_path}")

    return df


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Adult Experiment")
    parser.add_argument(
        "--method",
        default="mixup",
        type=str,
        help="erm/mixup/GapReg/fliprate/adversarial/reweight",
    )
    parser.add_argument("--mode", default="dp", type=str, help="dp/eo")
    parser.add_argument(
        "--lam", default=0.5, type=float, help="Lambda for regularization"
    )
    parser.add_argument("--num_exp", default=10, type=int, help="Number of experiments")
    parser.add_argument(
        "--dataset",
        default="adult",
        type=str,
        help="Dataset to use: adult/interpolation/synthetic/synthetic2",
    )
    parser.add_argument(
        "--synthetic2_test_k",
        default=1,
        type=float,
        help="Select synthetic2 test set: 0.5/1/1.5/2/2.5/3/3.5/4",
    )
    parser.add_argument(
        "--preprocess",
        default="",
        choices=["", "fairshift"],
        dest="pre_process",
        help="预处理方法，默认不使用；设置为 fairshift 可先对训练集进行 Fair-Shift",
    )
    parser.add_argument(
        "--full_sweep",
        action="store_true",
        help="运行所有方法与所有 synthetic2_test_k 组合并保存结果",
    )
    parser.add_argument(
        "--output_csv",
        default=None,
        type=str,
        help="若指定则将汇总结果保存到该 CSV 路径",
    )
    parser.add_argument(
        "--methods",
        nargs="*",
        default=None,
        help="自定义方法列表，例如: --methods erm mixup GapReg",
    )
    parser.add_argument(
        "--k_values",
        nargs="*",
        type=float,
        default=None,
        help="自定义 synthetic2_test_k 列表，例如: --k_values 0.5 1 1.5",
    )
    parser.add_argument(
        "--preprocess_list",
        nargs="*",
        default=None,
        help="自定义预处理列表，例如: --preprocess_list " " fairshift",
    )
    args = parser.parse_args()

    if args.full_sweep:
        run_full_sweep(
            mode=args.mode,
            lam=args.lam,
            num_exp=args.num_exp,
            methods=args.methods,
            synthetic2_test_k_list=args.k_values,
            pre_process_list=args.preprocess_list,
            output_csv=args.output_csv,
        )
    else:
        run_experiments(
            args.method,
            args.mode,
            args.lam,
            args.num_exp,
            args.dataset,
            args.synthetic2_test_k,
            args.pre_process,
        )
