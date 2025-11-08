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
)
from model import Net, build_model
from utils import train_dp, evaluate_dp
from utils import train_eo, evaluate_eo
from fliprate import preprocess_high_impact_samples
from adversarial import AdversarialDebiasingTorch
from reweight import fit_reweight_predictor
from fairshift import align_train_with_target

import json
import ast
import itertools
import os
from concurrent.futures import ThreadPoolExecutor, as_completed
from copy import deepcopy
from datetime import datetime


def load_hyper_parameter_list(file_path="hyper_parameter_list"):
    """从文件中加载超参数配置表"""
    try:
        df = pd.read_csv(file_path)
        return df
    except FileNotFoundError:
        print(f"警告: 未找到超参数配置文件 {file_path}，使用默认配置")
        return None
    except Exception as e:
        print(f"警告: 读取超参数配置文件失败 {e}，使用默认配置")
        return None


def get_method_params(df, method):
    """获取指定方法的所有参数配置"""
    if df is None:
        return None
    method_params = df[df["method"] == method]
    if method_params.empty:
        return None

    params_config = {}
    for _, row in method_params.iterrows():
        param_name = row["param_name"]
        params_config[param_name] = {
            "type": row["param_type"],
            "default": row["default_value"],
            "search_space": ast.literal_eval(row["search_space"]),
            "search_step": row["search_step"],
        }
    return params_config


def get_default_params(df, method):
    """获取指定方法的默认参数值"""
    params_config = get_method_params(df, method)
    if params_config is None:
        return {}

    defaults = {}
    for param_name, config in params_config.items():
        defaults[param_name] = config["default"]
    return defaults


def get_search_space(df, method):
    """获取指定方法的搜索空间"""
    params_config = get_method_params(df, method)
    if params_config is None:
        return {}

    search_space = {}
    for param_name, config in params_config.items():
        space = config["search_space"]
        if len(space) > 1:  # 只有搜索空间大于1时才加入
            search_space[param_name] = space
    return search_space


def generate_random_search_configs(
    search_space, default_params, n_samples=20, random_seed=None
):
    """
    生成随机搜索的超参数配置列表
    
    Args:
        search_space: 搜索空间字典，key为参数名，value为可选值列表
        default_params: 默认参数字典
        n_samples: 随机采样的配置数量
        random_seed: 随机种子
        
    Returns:
        config_list: 随机生成的配置列表
    """
    if random_seed is not None:
        np.random.seed(random_seed)
    
    config_list = []
    keys = list(search_space.keys())
    
    for _ in range(n_samples):
        hp_conf = deepcopy(default_params)
        for key in keys:
            # 从每个参数的搜索空间中随机选择一个值
            value = np.random.choice(search_space[key])
            # 转换numpy类型为Python原生类型，以便JSON序列化
            if hasattr(value, 'item'):
                value = value.item()
            hp_conf[key] = value
        config_list.append(hp_conf)
    
    return config_list


def _parse_dataset_spec(dataset_option):
    """
    解析数据集选项，支持以下格式：
    - "adult" / "interpolation": 直接使用
    - "synthetic": 默认跑所有8个数据集 (k=0.5,1,1.5,2,2.5,3,3.5,4)
    - "synthetic_1" / "synthetic_1.5": 只跑指定k值的数据集
    - {"name": "synthetic", "test_k": 1.5}: 字典格式
    """
    if isinstance(dataset_option, dict):
        if "name" not in dataset_option:
            raise ValueError("dataset specification dict must include 'name'")
        dataset_name = str(dataset_option["name"]).lower()
        dataset_params = {
            key: value for key, value in dataset_option.items() if key != "name"
        }
    elif isinstance(dataset_option, (list, tuple)):
        if not dataset_option:
            raise ValueError("dataset specification tuple/list cannot be empty")
        dataset_name = str(dataset_option[0]).lower()
        dataset_params = {}
        if len(dataset_option) > 1:
            dataset_params["test_k"] = dataset_option[1]
    else:
        dataset_str = str(dataset_option)
        dataset_params = {}

        # 处理 synthetic_k 格式
        if dataset_str.lower().startswith("synthetic"):
            if dataset_str.lower() == "synthetic":
                # 只写 synthetic，返回 None 表示需要跑所有8个数据集
                dataset_name = "synthetic"
                dataset_params["test_k_list"] = [0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4]
            elif "_" in dataset_str:
                # synthetic_1 格式
                parts = dataset_str.split("_", 1)
                dataset_name = "synthetic"
                try:
                    k_value = float(parts[1])
                    dataset_params["test_k"] = k_value
                except ValueError as exc:
                    raise ValueError(
                        f"无法解析 dataset '{dataset_option}' 中的 k 值，格式应为 synthetic_k"
                    ) from exc
            else:
                dataset_name = dataset_str.lower()
        else:
            dataset_name = dataset_str.lower()

    # 如果是 synthetic 但没有指定 test_k 或 test_k_list，默认设置为单个值
    if dataset_name == "synthetic":
        if "test_k" not in dataset_params and "test_k_list" not in dataset_params:
            dataset_params["test_k"] = 1.0

    return dataset_name, dataset_params


def _format_train_kwargs(kwargs):
    if not kwargs:
        return "{}"
    return json.dumps(kwargs, sort_keys=True)


def _format_hp(hp):
    if not hp:
        return "{}"
    return json.dumps(dict(sorted(hp.items())), sort_keys=True)


def _generate_result_filename(methods, mode, datasets, preprocess_list, search_hp):
    """生成带时间戳和参数的结果文件名"""
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

    # 方法名称
    if isinstance(methods, list) and len(methods) > 1:
        method_str = f"{len(methods)}methods"
    elif isinstance(methods, list) and len(methods) == 1:
        method_str = methods[0]
    else:
        method_str = str(methods)

    # 数据集信息
    if isinstance(datasets, list) and len(datasets) > 1:
        dataset_str = f"{len(datasets)}datasets"
    elif isinstance(datasets, list) and len(datasets) == 1:
        dataset_str = datasets[0]
    else:
        dataset_str = str(datasets)

    # 预处理
    if preprocess_list and len(preprocess_list) > 1:
        preprocess_str = "multiprep"
    elif preprocess_list and len(preprocess_list) == 1:
        preprocess_str = preprocess_list[0] if preprocess_list[0] else "noprep"
    else:
        preprocess_str = "noprep"

    # 超参搜索
    hp_str = "hpsearch" if search_hp else "default"

    # 组合文件名
    filename = f"results_{method_str}_{mode}_{dataset_str}_{preprocess_str}_{hp_str}_{timestamp}.csv"

    return filename


def run_experiments(
    method="mixup",
    mode="dp",
    num_exp=10,
    dataset_spec="adult",
    pre_process="",
    model_arch="mlp",
    search_hp=False,
    hp_selection_metric="gap",
    hp_config_df=None,
    hp_search_method="random",
    hp_random_samples=20,
    hp_random_seed=None,
):
    """
    对模型进行 num_exp 次重复实验并返回性能统计

    Args:
        method: 训练方法
        mode: 公平性模式 (dp/eo)
        num_exp: 实验重复次数
        dataset_spec: 数据集描述，可为字符串、列表或字典；若为 synthetic，可携带 test_k
        pre_process: 预处理方法
        model_arch: 模型架构
        search_hp: 是否进行超参数搜索
        hp_selection_metric: 超参选择指标 (gap/ap)
        hp_config_df: 超参数配置表 DataFrame
        hp_search_method: 超参数搜索方法 ("random"或"grid")，默认"random"
        hp_random_samples: 随机搜索时的采样数量，默认20
        hp_random_seed: 随机搜索的随机种子，默认None
    
    Returns:
        dict: 包含所有配置的实验结果、最佳结果和最佳超参数
    """

    dataset, dataset_params = _parse_dataset_spec(dataset_spec)

    # 处理 test_k_list：如果不是批量运行模式，只使用第一个 k 值
    if "test_k_list" in dataset_params and "test_k" not in dataset_params:
        test_k_list = dataset_params["test_k_list"]
        dataset_params["test_k"] = test_k_list[0]
        print(f"提示: dataset='synthetic' 会默认使用 test_k={test_k_list[0]}。")
        print(f"      如需运行所有 k 值 {test_k_list}，请使用 --full_sweep 模式。")
        print(
            f"      如需指定单个 k 值，请使用 --dataset synthetic_k 格式（如 synthetic_1）。\n"
        )

    # 加载超参数配置
    if hp_config_df is None:
        hp_config_df = load_hyper_parameter_list()

    # 获取方法的默认参数
    default_params = get_default_params(hp_config_df, method)

    # 根据是否搜索超参来决定配置列表
    if search_hp:
        # 获取搜索空间
        search_space = get_search_space(hp_config_df, method)
        if not search_space:
            # 如果没有搜索空间，使用默认配置
            config_list = [default_params]
            print(f"未找到 {method} 的搜索空间，使用默认配置")
        else:
            # 根据搜索方法生成配置列表
            if hp_search_method == "grid":
                # 网格搜索：生成所有参数组合
                print(f"使用网格搜索方法生成超参数配置...")
                config_list = []
                keys = list(search_space.keys())
                values_product = itertools.product(*(search_space[k] for k in keys))
                for values in values_product:
                    hp_conf = deepcopy(default_params)
                    for key, value in zip(keys, values):
                        hp_conf[key] = value
                    config_list.append(hp_conf)
                print(f"网格搜索生成了 {len(config_list)} 个配置")
            elif hp_search_method == "random":
                # 随机搜索：随机采样配置
                print(f"使用随机搜索方法生成超参数配置...")
                config_list = generate_random_search_configs(
                    search_space, 
                    default_params, 
                    n_samples=hp_random_samples,
                    random_seed=hp_random_seed
                )
                print(f"随机搜索生成了 {len(config_list)} 个配置")
            else:
                raise ValueError(f"不支持的超参数搜索方法: {hp_search_method}. 请使用 'grid' 或 'random'")
    else:
        # 不搜索，只使用默认配置
        config_list = [default_params]

    best_result = None
    best_metric_value = None
    best_hp = None
    all_results = []

    max_workers = min(len(config_list), max(1, (os.cpu_count() or 1)))
    futures = {}
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        for hp_conf in config_list:
            future = executor.submit(
                _run_single_experiment,
                method,
                mode,
                hp_conf,
                num_exp,
                dataset,
                deepcopy(dataset_params),
                pre_process,
                model_arch,
            )
            futures[future] = hp_conf

        for future in as_completed(futures):
            hp_conf = futures[future]
            summary = future.result()
            summary["hp"] = hp_conf
            all_results.append(summary)

            # 根据选择指标确定评价值
            # gap: 越小越好（最小化公平性差距）
            # ap: 越大越好（最大化准确率）
            if hp_selection_metric == "gap":
                metric_value = summary.get("gap_mean")
                ap_value = summary.get("ap_mean")
                compare_value = metric_value  # 越小越好
                
                # 当以gap为目标时，要求准确率ap至少0.75以上
                if ap_value is None or ap_value < 0.75:
                    print(f"  跳过配置 ")
                    continue
            else:  # hp_selection_metric == "ap"
                metric_value = summary.get("ap_mean")
                compare_value = -metric_value  # 转负数，越大越好变成越小越好

            if metric_value is None:
                continue

            if best_metric_value is None or compare_value < best_metric_value:
                best_metric_value = compare_value
                best_result = summary
                best_hp = hp_conf

    if best_result:
        print("\n" + "=" * 80)
        print("最佳超参数组合")
        print("=" * 80)
        print(f"方法: {best_result.get('method')}")
        print(f"数据集: {best_result.get('dataset')}", end="")
        if best_result.get("synthetic_test_k"):
            print(f" (test_k={best_result.get('synthetic_test_k')})", end="")
        print()
        print(
            f"预处理: {best_result.get('pre_process') if best_result.get('pre_process') else '无'}"
        )
        print(
            f"选择指标: {hp_selection_metric} ({'最小化' if hp_selection_metric == 'gap' else '最大化'})"
        )
        if hp_selection_metric == "gap":
            print(f"筛选条件: AP ≥ 0.5")
        print("-" * 80)
        print("最佳超参数:")
        for key, value in sorted(best_hp.items()):
            print(f"  {key}: {value}")
        print("-" * 80)
        print("性能表现:")
        print(
            f"  Average Precision: {best_result.get('ap_mean'):.4f} ± {best_result.get('ap_std', 0):.4f}"
        )
        print(
            f"  Gap: {best_result.get('gap_mean'):.4f} ± {best_result.get('gap_std', 0):.4f}"
        )
        print("=" * 80 + "\n")
    else:
        print("\n" + "=" * 80)
        print("警告: 未找到满足条件的超参数组合")
        print("=" * 80)
        if hp_selection_metric == "gap":
            print("所有配置的AP均低于0.5的阈值要求")
            print("建议: 尝试调整搜索空间或降低AP阈值")
        print("=" * 80 + "\n")

    return {
        "results": all_results,
        "best_result": best_result,
        "best_hp": best_hp,
        "hp_selection_metric": hp_selection_metric,
    }


def _summarize_hp_search(results, metric):
    """打印超参数搜索结果的汇总信息"""
    best = results.get("best_result")
    if not best:
        print("\n未找到有效的超参组合\n")
        return

    best_hp = best.get("hp")
    print("\n" + "=" * 80)
    print("超参数搜索结果汇总")
    print("=" * 80)
    print(f"方法: {best.get('method')}")
    print(f"模式: {best.get('mode')}")
    print(f"数据集: {best.get('dataset')}", end="")
    if best.get("synthetic_test_k"):
        print(f" (test_k={best.get('synthetic_test_k')})", end="")
    print()
    print(f"预处理: {best.get('pre_process') if best.get('pre_process') else '无'}")
    print(f"选择指标: {metric} ({'最小化' if metric == 'gap' else '最大化'})")
    if metric == "gap":
        print(f"筛选条件: AP ≥ 0.5")
    print("-" * 80)
    print("最佳超参数配置:")
    for key, value in sorted(best_hp.items()):
        print(f"  {key}: {value}")
    print("-" * 80)
    print("性能表现:")
    print(
        f"  Average Precision: {best.get('ap_mean'):.4f} ± {best.get('ap_std', 0):.4f}"
    )
    print(f"  Gap: {best.get('gap_mean'):.4f} ± {best.get('gap_std', 0):.4f}")
    print("=" * 80 + "\n")


def _run_single_experiment(
    method,
    mode,
    hp_conf,
    num_exp,
    dataset,
    dataset_params,
    pre_process,
    model_arch,
):
    """针对给定的超参数配置运行单次实验"""

    # hp_conf 现在包含所有参数（包括训练参数和超参数）
    hp_conf = deepcopy(hp_conf) if hp_conf else {}

    # 打印当前实验配置
    print(f"\n>>> 开始实验: method={method}, mode={mode}, hp={_format_hp(hp_conf)}")

    # 提取训练参数
    optimizer_lr = hp_conf.get("lr", 1e-3)
    eval_epochs = int(hp_conf.get("eval_epochs", 10))
    eval_epochs = max(1, eval_epochs)
    skip_initial_epochs = int(hp_conf.get("skip_initial_epochs", 1))
    skip_initial_epochs = min(max(0, skip_initial_epochs), eval_epochs - 1)

    training_params = {
        "batch_size": int(hp_conf.get("batch_size", 500)),
        "niter": int(hp_conf.get("niter", 100)),
        "high_impact_ratio": float(
            hp_conf.get(
                "high_impact_ratio",
                0.2 if method in {"fliprate", "fliprate-fairshift"} else 0.04,
            )
        ),
    }

    final_train_config = {
        "lr": optimizer_lr,
        "eval_epochs": eval_epochs,
        "skip_initial_epochs": skip_initial_epochs,
        **training_params,
    }

    lam = float(hp_conf.get("lam", 0.5))

    # 如果使用fliprate方法，需要预处理高影响样本
    use_high_impact = method in {"fliprate", "fliprate-fairshift"}
    apply_fairshift = method == "fliprate-fairshift"
    high_impact_ratio = training_params.get(
        "high_impact_ratio", 0.2 if use_high_impact else 0.04
    )
    synthetic_test_k = dataset_params.get("test_k", 1.0)

    # Fliprate 特有的超参数
    pair_count_K = int(hp_conf.get("pair_count_K", 200))
    gamma_samples = int(hp_conf.get("gamma_samples", 10))

    if use_high_impact:
        print("Using fliprate method with high impact samples...")
        print(
            f"Fliprate parameters: pair_count_K={pair_count_K}, gamma_samples={gamma_samples}"
        )
        print("Preprocessing high impact samples for all seeds...")
        for i in range(num_exp):
            preprocess_high_impact_samples(
                seed=i,
                top_k=1000,
                force_recompute=apply_fairshift,
                dataset=dataset,
                mode=mode,
                synthetic_test_k=synthetic_test_k,
                apply_fairshift=apply_fairshift,
                high_impact_strategy="fairshift" if apply_fairshift else "default",
                K=pair_count_K,
                gamma_samples=gamma_samples,
            )

    ap = []
    gap = []
    gap_mean = []

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
                preprocess_synthetic_data(seed=i, test_k=synthetic_test_k)
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
                ap_val, gap_val, mf_val, gap_mean_val = evaluate_eo(
                    predictor, X_val, y_val, A_val
                )
                ap_test, gap_test, mf_test, gap_mean_test = evaluate_eo(
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
            # 提取 reweight 特有的超参数
            alpha = float(hp_conf.get("alpha", 0.1))
            beta = float(hp_conf.get("beta", 1.0))
            gamma = float(hp_conf.get("gamma", 1.0))

            predictor = fit_reweight_predictor(
                X_train,
                y_train,
                A_train,
                X_val,
                y_val,
                A_val,
                mode=mode,
                lam=lam,
                alpha=alpha,
                beta=beta,
                gamma=gamma,
                random_state=i,
            )

            if mode == "dp":
                ap_val, gap_val = evaluate_dp(predictor, X_val, y_val, A_val)
                ap_test, gap_test = evaluate_dp(predictor, X_test, y_test, A_test)
            elif mode == "eo":
                ap_val, gap_val, mf_val, gap_mean_val = evaluate_eo(
                    predictor, X_val, y_val, A_val
                )
                ap_test, gap_test, mf_test, gap_mean_test = evaluate_eo(
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

        # 加载高影响样本数据（如果使用fliprate方法）
        high_impact_data_for_training = None
        if use_high_impact:
            from fliprate import load_and_select_high_impact_samples
            
            # 构建数据文件路径
            suffix = ""
            if dataset == "synthetic":
                k_value = float(synthetic_test_k)
                k_formatted = f"{k_value:.2f}"
                k_formatted = k_formatted.rstrip("0").rstrip(".")
                suffix = f"_k{k_formatted}"
            strategy_suffix = (
                f"_{'fairshift' if apply_fairshift else 'default'}"
                if apply_fairshift
                else ""
            )
            hyperparam_suffix = f"_K{pair_count_K}_g{gamma_samples}"
            data_filepath = f"tmp/fliprate_highimpact_sample/all_sample_data_seed{i}_{mode}_{dataset}{suffix}{strategy_suffix}{hyperparam_suffix}.pkl"
            
            # 加载数据
            high_impact_data_for_training = load_and_select_high_impact_samples(
                data_filepath, top_k=1000
            )
            if high_impact_data_for_training is None:
                print(f"Warning: High impact data file {data_filepath} not found")
        
        # initialize model
        model = build_model(model_arch, input_size=len(X_train[0])).cuda()
        optimizer = optim.Adam(model.parameters(), lr=optimizer_lr)
        criterion = nn.BCELoss()

        # run experiments
        ap_val_epoch = []
        gap_val_epoch = []
        gap_mean_val_epoch = []
        ap_test_epoch = []
        gap_test_epoch = []
        gap_mean_test_epoch = []
        mf_test_epoch = [] if mode == "eo" else None
        for j in tqdm(range(eval_epochs)):
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
                    batch_size=training_params.get("batch_size"),
                    niter=training_params.get("niter"),
                    high_impact_data=high_impact_data_for_training,
                    high_impact_ratio=training_params.get(
                        "high_impact_ratio", high_impact_ratio
                    ),
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
                    batch_size=training_params.get("batch_size"),
                    niter=training_params.get("niter"),
                    high_impact_data=high_impact_data_for_training,
                    high_impact_ratio=training_params.get(
                        "high_impact_ratio", high_impact_ratio
                    ),
                )
                ap_val, gap_val, mf_val, gap_mean_val = evaluate_eo(
                    model, X_val, y_val, A_val
                )
                ap_test, gap_test, mf_test, gap_mean_test = evaluate_eo(
                    model, X_test, y_test, A_test
                )

            if j > skip_initial_epochs:  # 跳过初始epoch的结果
                ap_val_epoch.append(ap_val)
                ap_test_epoch.append(ap_test)
                gap_val_epoch.append(gap_val)
                gap_test_epoch.append(gap_test)
                if mode == "eo":
                    gap_mean_val_epoch.append(gap_mean_val)
                    gap_mean_test_epoch.append(gap_mean_test)
                    mf_test_epoch.append(mf_test)

        # best model based on validation performance
        idx = gap_val_epoch.index(min(gap_val_epoch))
        gap.append(gap_test_epoch[idx])
        ap.append(ap_test_epoch[idx])
        if mode == "eo":
            gap_mean.append(gap_mean_test_epoch[idx])
            if mf_test_epoch:
                mf_results.append(mf_test_epoch[idx])

    ap_mean = float(np.mean(ap)) if ap else float("nan")
    gap_avg = float(np.mean(gap)) if gap else float("nan")
    gap_mean_mean = float(np.mean(gap_mean)) if gap_mean else float("nan")
    ap_std = float(np.std(ap)) if ap else float("nan")
    gap_std = float(np.std(gap)) if gap else float("nan")

    print("\n" + "=" * 80)
    print(f"实验结果汇总")
    print("=" * 80)
    print(f"方法: {method}")
    print(f"模式: {mode}")
    print(
        f"数据集: {dataset}"
        + (
            f" (test_k={dataset_params.get('test_k')})"
            if dataset == "synthetic"
            else ""
        )
    )
    print(f"预处理: {pre_process if pre_process else '无'}")
    print(f"实验次数: {num_exp}")
    print("-" * 80)
    print("超参数配置:")
    for key, value in sorted(hp_conf.items()):
        print(f"  {key}: {value}")
    print("-" * 80)
    print("性能指标:")
    print(f"  Average Precision: {ap_mean:.4f} ± {ap_std:.4f}")
    print(f"  {mode.upper()} Gap: {gap_avg:.4f} ± {gap_std:.4f}")
    if mode == "eo" and gap_mean:
        print(f"  {mode.upper()} Gap(mean): {gap_mean_mean:.4f}")
    print("=" * 80 + "\n")

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
        "hp": hp_conf,
        "hp_repr": _format_hp(hp_conf),
        "num_exp": num_exp,
        "dataset": dataset,
        "synthetic_test_k": dataset_params.get("test_k"),
        "pre_process": pre_process,
        "ap_mean": ap_mean,
        "gap_mean": gap_avg,
        "ap_std": ap_std,
        "gap_std": gap_std,
        "train_config": final_train_config,
        "train_config_repr": _format_train_kwargs(final_train_config),
    }
    if mf_group_avg_json is not None:
        result_summary["mf_by_group_mean"] = mf_group_avg_json

    return result_summary


def run_full_sweep(
    mode="dp",
    num_exp=10,
    methods=None,
    synthetic_test_k_list=None,
    pre_process_list=None,
    output_csv=None,
    search_hp=False,
    hp_selection_metric="gap",
    datasets_list=None,
    hp_search_method="random",
    hp_random_samples=20,
    hp_random_seed=None,
):
    """
    批量运行所有方法与测试集组合，并保存结果
    
    Args:
        mode: 公平性模式 (dp/eo)
        num_exp: 实验重复次数
        methods: 训练方法列表
        synthetic_test_k_list: synthetic数据集的k值列表
        pre_process_list: 预处理方法列表
        output_csv: 输出CSV文件路径
        search_hp: 是否进行超参数搜索
        hp_selection_metric: 超参选择指标 (gap/ap)
        datasets_list: 数据集列表
        hp_search_method: 超参数搜索方法 ("random"或"grid")，默认"random"
        hp_random_samples: 随机搜索时的采样数量，默认20
        hp_random_seed: 随机搜索的随机种子，默认None
    
    Returns:
        DataFrame: 包含所有实验结果的数据框
    """

    if methods is None:
        methods = ["erm", "mixup", "GapReg", "fliprate", "adversarial", "reweight"]
    if pre_process_list is None:
        pre_process_list = ["", "fairshift"]

    # 如果提供了 datasets_list，使用它；否则使用 synthetic_test_k_list
    if datasets_list is None:
        if synthetic_test_k_list is None:
            synthetic_test_k_list = [0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4]
        datasets_list = [f"synthetic_{k}" for k in synthetic_test_k_list]

    # 加载超参数配置
    hp_config_df = load_hyper_parameter_list()

    results = []
    best_records = []

    for pre_process in pre_process_list:
        for dataset_spec in datasets_list:
            for method in methods:
                display_preprocess = pre_process if pre_process else "无"
                print("\n" + "#" * 80)
                print(f"批量实验 - 开始新配置")
                print("#" * 80)
                print(f"方法: {method}")
                print(f"数据集: {dataset_spec}")
                print(f"预处理: {display_preprocess}")
                print(f"超参数搜索: {'是' if search_hp else '否'}")
                if search_hp:
                    print(f"搜索方法: {hp_search_method}")
                    if hp_search_method == "random":
                        print(f"随机采样数: {hp_random_samples}")
                print(f"选择指标: {hp_selection_metric}")
                print("#" * 80 + "\n")
                experiment_summary = run_experiments(
                    method=method,
                    mode=mode,
                    num_exp=num_exp,
                    dataset_spec=dataset_spec,
                    pre_process=pre_process,
                    model_arch="mlp",
                    search_hp=search_hp,
                    hp_selection_metric=hp_selection_metric,
                    hp_config_df=hp_config_df,
                    hp_search_method=hp_search_method,
                    hp_random_samples=hp_random_samples,
                    hp_random_seed=hp_random_seed,
                )

                best_result = experiment_summary.get("best_result")
                if best_result:
                    best_records.append(best_result)
                for summary in experiment_summary.get("results", []):
                    row = {
                        "method": summary.get("method", method),
                        "mode": mode,
                        "hp_repr": _format_hp(summary.get("hp")),
                        "num_exp": num_exp,
                        "dataset": summary.get("dataset"),
                        "synthetic_test_k": summary.get("synthetic_test_k"),
                        "pre_process": display_preprocess,
                        "ap_mean": summary.get("ap_mean"),
                        "gap_mean": summary.get("gap_mean"),
                        "ap_std": summary.get("ap_std"),
                        "gap_std": summary.get("gap_std"),
                        "hp_selection_metric": hp_selection_metric,
                    }
                    if "mf_by_group_mean" in summary:
                        row["mf_by_group_mean"] = summary["mf_by_group_mean"]
                    results.append(row)

    if best_records:
        print("\n" + "=" * 80)
        print("批量实验 - 最佳超参结果汇总")
        print("=" * 80)
        for i, record in enumerate(best_records, 1):
            print(f"\n配置 {i}:")
            print(f"  方法: {record['method']}")
            print(f"  Test_k: {record['synthetic_test_k']}")
            print(
                f"  预处理: {record['pre_process'] if record['pre_process'] else '无'}"
            )
            print(f"  最佳超参: {_format_hp(record.get('hp'))}")
            print(f"  Average Precision: {record.get('ap_mean'):.4f}")
            print(f"  Gap: {record.get('gap_mean'):.4f}")
        print("\n" + "=" * 80 + "\n")

    df = pd.DataFrame(results)
    # 根据数据列动态决定排序列
    sort_cols = []
    if "pre_process" in df.columns:
        sort_cols.append("pre_process")
    if "dataset" in df.columns:
        sort_cols.append("dataset")
    if "synthetic_test_k" in df.columns:
        sort_cols.append("synthetic_test_k")
    if "method" in df.columns:
        sort_cols.append("method")
    if "hp_repr" in df.columns:
        sort_cols.append("hp_repr")

    if sort_cols:
        df.sort_values(sort_cols, inplace=True)

    # 自动生成文件名（如果未指定）
    if output_csv is None:
        output_filename = _generate_result_filename(
            methods=methods,
            mode=mode,
            datasets=datasets_list,
            preprocess_list=pre_process_list,
            search_hp=search_hp,
        )
        output_path = Path("adult/results") / output_filename
    else:
        output_path = Path(output_csv)

    output_path.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(output_path, index=False)

    # print("\n汇总结果：")
    # print(df.to_string(index=False))
    print(f"\n结果已保存到: {output_path}")

    return df


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="AI公平性算法测试实验")
    parser.add_argument(
        "--method",
        default=None,
        type=str,
        help="单个训练方法: erm/mixup/GapReg/fliprate/adversarial/reweight",
    )
    parser.add_argument(
        "--methods",
        nargs="*",
        default=None,
        help="多个训练方法列表，例如: --methods erm mixup GapReg",
    )
    parser.add_argument("--mode", default="eo", type=str, help="公平性模式: dp/eo")
    parser.add_argument("--num_exp", default=3, type=int, help="实验重复次数，默认10次")
    parser.add_argument(
        "--dataset",
        default=None,
        type=str,
        help="单个数据集: adult/interpolation/synthetic/synthetic_1/synthetic_1.5 等",
    )
    parser.add_argument(
        "--k_values",
        nargs="*",
        type=float,
        default=None,
        help="Synthetic 数据集的 k 值列表，例如: --k_values 0.5 1 1.5",
    )
    parser.add_argument(
        "--preprocess",
        default=None,
        type=str,
        choices=[None, "", "fairshift"],
        dest="pre_process",
        help="单个预处理方法: 空字符串(不使用)/fairshift",
    )
    parser.add_argument(
        "--preprocess_list",
        nargs="*",
        default=None,
        help='多个预处理方法列表，例如: --preprocess_list "" fairshift',
    )
    parser.add_argument(
        "--model_arch",
        default="mlp",
        choices=["mlp", "net", "logistic", "logreg", "lr"],
        help="模型结构: mlp(默认)/logistic",
    )
    parser.add_argument(
        "--search_hp",
        action="store_true",
        help="是否进行超参数搜索（从 hyper_parameter_list 文件中读取搜索空间）",
    )
    parser.add_argument(
        "--hp_selection_metric",
        default="gap",
        choices=["gap", "ap"],
        help="超参搜索目标指标: gap(最小化)/ap(最大化)",
    )
    parser.add_argument(
        "--hp_search_method",
        default="random",
        choices=["random", "grid"],
        help="超参数搜索方法: random(随机搜索，默认)/grid(网格搜索)",
    )
    parser.add_argument(
        "--hp_random_samples",
        default=20,
        type=int,
        help="随机搜索时采样的配置数量，默认20",
    )
    parser.add_argument(
        "--hp_random_seed",
        default=None,
        type=int,
        help="随机搜索的随机种子，默认None（不固定种子）",
    )
    parser.add_argument(
        "--output_csv",
        default=None,
        type=str,
        help="结果保存的 CSV 文件路径（不指定则自动生成）",
    )
    args = parser.parse_args()

    # 确定方法列表
    if args.methods:
        methods_list = args.methods
    elif args.method:
        methods_list = [args.method]
    else:
        methods_list = ["mixup"]  # 默认方法

    # 确定数据集列表
    datasets_list = []
    if args.k_values:
        # 使用 k_values 指定多个 synthetic 数据集
        for k in args.k_values:
            datasets_list.append(f"synthetic_{k}")
    elif args.dataset:
        datasets_list = [args.dataset]
    else:
        datasets_list = ["adult"]  # 默认数据集

    # 确定预处理列表
    if args.preprocess_list:
        preprocess_list = args.preprocess_list
    elif args.pre_process is not None:
        preprocess_list = [args.pre_process]
    else:
        preprocess_list = [""]  # 默认不使用预处理

    # 判断是单次运行还是批量运行
    is_batch = (
        len(methods_list) > 1 or len(datasets_list) > 1 or len(preprocess_list) > 1
    )

    if is_batch:
        # 批量运行模式
        print("\n" + "=" * 80)
        print("批量运行模式")
        print("=" * 80)
        print(f"方法列表: {methods_list}")
        print(f"数据集列表: {datasets_list}")
        print(f"预处理列表: {preprocess_list}")
        print(f"超参数搜索: {'是' if args.search_hp else '否'}")
        if args.search_hp:
            print(f"搜索方法: {args.hp_search_method}")
            if args.hp_search_method == "random":
                print(f"随机采样数: {args.hp_random_samples}")
        print("=" * 80 + "\n")

        run_full_sweep(
            mode=args.mode,
            num_exp=args.num_exp,
            methods=methods_list,
            synthetic_test_k_list=args.k_values if args.k_values else None,
            pre_process_list=preprocess_list,
            output_csv=args.output_csv,
            search_hp=args.search_hp,
            hp_selection_metric=args.hp_selection_metric,
            datasets_list=datasets_list,
            hp_search_method=args.hp_search_method,
            hp_random_samples=args.hp_random_samples,
            hp_random_seed=args.hp_random_seed,
        )
    else:
        # 单次运行模式
        print("\n" + "=" * 80)
        print("单次运行模式")
        print("=" * 80)
        print(f"方法: {methods_list[0]}")
        print(f"数据集: {datasets_list[0]}")
        print(f"预处理: {preprocess_list[0] if preprocess_list[0] else '无'}")
        print(f"超参数搜索: {'是' if args.search_hp else '否'}")
        if args.search_hp:
            print(f"搜索方法: {args.hp_search_method}")
            if args.hp_search_method == "random":
                print(f"随机采样数: {args.hp_random_samples}")
        print("=" * 80 + "\n")

        experiment_summary = run_experiments(
            method=methods_list[0],
            mode=args.mode,
            num_exp=args.num_exp,
            dataset_spec=datasets_list[0],
            pre_process=preprocess_list[0],
            model_arch=args.model_arch,
            search_hp=args.search_hp,
            hp_selection_metric=args.hp_selection_metric,
            hp_search_method=args.hp_search_method,
            hp_random_samples=args.hp_random_samples,
            hp_random_seed=args.hp_random_seed,
        )
        _summarize_hp_search(experiment_summary, args.hp_selection_metric)
        
        # 保存结果到CSV文件
        method = methods_list[0]
        display_preprocess = preprocess_list[0] if preprocess_list[0] else "无"
        results = []
        
        for summary in experiment_summary.get("results", []):
            row = {
                "method": summary.get("method", method),
                "mode": args.mode,
                "hp_repr": _format_hp(summary.get("hp")),
                "num_exp": args.num_exp,
                "dataset": summary.get("dataset"),
                "synthetic_test_k": summary.get("synthetic_test_k"),
                "pre_process": display_preprocess,
                "ap_mean": summary.get("ap_mean"),
                "gap_mean": summary.get("gap_mean"),
                "ap_std": summary.get("ap_std"),
                "gap_std": summary.get("gap_std"),
                "hp_selection_metric": args.hp_selection_metric,
            }
            if "mf_by_group_mean" in summary:
                row["mf_by_group_mean"] = summary["mf_by_group_mean"]
            results.append(row)
        
        if results:
            df = pd.DataFrame(results)
            
            # 根据数据列动态决定排序列
            sort_cols = []
            if "pre_process" in df.columns:
                sort_cols.append("pre_process")
            if "dataset" in df.columns:
                sort_cols.append("dataset")
            if "synthetic_test_k" in df.columns:
                sort_cols.append("synthetic_test_k")
            if "method" in df.columns:
                sort_cols.append("method")
            if "hp_repr" in df.columns:
                sort_cols.append("hp_repr")
            
            if sort_cols:
                df.sort_values(sort_cols, inplace=True)
            
            # 自动生成文件名（如果未指定）
            if args.output_csv is None:
                output_filename = _generate_result_filename(
                    methods=methods_list,
                    mode=args.mode,
                    datasets=datasets_list,
                    preprocess_list=preprocess_list,
                    search_hp=args.search_hp,
                )
                output_path = Path("adult/results") / output_filename
            else:
                output_path = Path(args.output_csv)
            
            output_path.parent.mkdir(parents=True, exist_ok=True)
            df.to_csv(output_path, index=False)
            
            print(f"\n{'='*80}")
            print(f"结果已保存到: {output_path}")
            print(f"{'='*80}\n")