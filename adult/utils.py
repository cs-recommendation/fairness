import torch
import numpy as np
from numpy.random import beta
from sklearn.metrics import average_precision_score
from tqdm import tqdm

import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.autograd import Variable
from fairlearn.metrics import demographic_parity_difference, equalized_odds_difference
from fliprate import load_and_select_high_impact_samples, ensure_high_impact_feature_dim

from fairlearn.metrics import MetricFrame
from fairlearn.metrics import count, false_positive_rate, selection_rate
from sklearn.metrics import recall_score

# 检测设备
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")


def sample_batch_sen_idx(X, A, y, batch_size, s):
    candidates = np.where(A == s)[0]
    if len(candidates) == 0:
        raise ValueError(f"No samples found for sensitive attribute value {s}")
    replace_flag = len(candidates) < batch_size
    batch_idx = np.random.choice(
        candidates, size=batch_size, replace=replace_flag
    ).tolist()
    batch_x = X[batch_idx]
    batch_y = y[batch_idx]
    batch_x = torch.tensor(batch_x).to(device).float()
    batch_y = torch.tensor(batch_y).to(device).float()

    return batch_x, batch_y


def sample_batch_sen_idx_y(X, A, y, batch_size, s):
    batch_idx = []
    for i in range(2):
        idx = list(set(np.where(A == s)[0]) & set(np.where(y == i)[0]))
        if len(idx) == 0:
            raise ValueError(f"No samples found for sensitive={s}, label={i}")
        replace_flag = len(idx) < batch_size
        batch_idx += np.random.choice(
            idx, size=batch_size, replace=replace_flag
        ).tolist()

    batch_x = X[batch_idx]
    batch_y = y[batch_idx]
    batch_x = torch.tensor(batch_x).to(device).float()
    batch_y = torch.tensor(batch_y).to(device).float()

    return batch_x, batch_y


def sample_batch_with_high_impact(
    X, A, y, batch_size, s, high_impact_data=None, high_impact_ratio=0.04
):
    """
    从指定敏感属性的样本中采样batch，并将一部分替换为高影响样本

    Args:
        X, A, y: 训练数据
        batch_size: batch大小
        s: 敏感属性值
        high_impact_data: 高影响样本数据
        high_impact_ratio: 高影响样本在batch中的比例

    Returns:
        batch_x, batch_y: 混合了高影响样本的batch
    """
    if high_impact_data is None:
        # 如果没有高影响样本数据，使用原始采样方法
        return sample_batch_sen_idx(X, A, y, batch_size, s)

    # 计算高影响样本数量
    high_impact_size = int(batch_size * high_impact_ratio)
    normal_size = batch_size - high_impact_size

    # 从正常样本中采样
    normal_idx = np.where(A == s)[0]
    if len(normal_idx) >= normal_size:
        normal_batch_idx = np.random.choice(
            normal_idx, size=normal_size, replace=False
        ).tolist()
    else:
        normal_batch_idx = np.random.choice(
            normal_idx, size=normal_size, replace=True
        ).tolist()

    # 从高影响样本中采样（只选择对应敏感属性的样本）
    high_impact_A = high_impact_data["A"]
    high_impact_indices = np.where(high_impact_A == s)[0]

    if len(high_impact_indices) >= high_impact_size:
        selected_high_impact_idx = np.random.choice(
            high_impact_indices, size=high_impact_size, replace=False
        )
    else:
        selected_high_impact_idx = np.random.choice(
            high_impact_indices, size=high_impact_size, replace=True
        )

    # 组合batch
    normal_x = X[normal_batch_idx]
    normal_y = y[normal_batch_idx]

    high_impact_x = high_impact_data["X"][selected_high_impact_idx]
    high_impact_y = high_impact_data["y"][selected_high_impact_idx]

    # 维度一致性检查，若不一致则回退到普通采样，避免报错
    if normal_x.shape[1] != high_impact_x.shape[1]:
        print(
            "[warn] Feature dim mismatch between normal and high-impact samples; fallback to normal sampling."
        )
        return sample_batch_sen_idx(X, A, y, batch_size, s)

    # 合并数据
    batch_x = np.concatenate([normal_x, high_impact_x], axis=0)
    batch_y = np.concatenate([normal_y, high_impact_y], axis=0)

    # 转换为tensor
    batch_x = torch.tensor(batch_x).to(device).float()
    batch_y = torch.tensor(batch_y).to(device).float()

    return batch_x, batch_y


def sample_batch_sen_idx_y_with_high_impact(
    X, A, y, batch_size, s, high_impact_data=None, high_impact_ratio=0.04
):
    """
    按敏感属性和标签采样batch，并混合高影响样本
    """
    if high_impact_data is None:
        return sample_batch_sen_idx_y(X, A, y, batch_size, s)

    # 计算高影响样本数量
    high_impact_size = int(batch_size * high_impact_ratio)
    normal_size = batch_size - high_impact_size

    # 从正常样本中按标签采样
    batch_idx = []
    for i in range(2):
        idx = list(set(np.where(A == s)[0]) & set(np.where(y == i)[0]))
        if len(idx) >= normal_size:
            batch_idx += np.random.choice(idx, size=normal_size, replace=False).tolist()
        else:
            batch_idx += np.random.choice(idx, size=normal_size, replace=True).tolist()

    # 从高影响样本中采样
    high_impact_A = high_impact_data["A"]
    high_impact_indices = np.where(high_impact_A == s)[0]

    if len(high_impact_indices) >= high_impact_size * 2:  # 为每个标签采样
        selected_high_impact_idx = np.random.choice(
            high_impact_indices, size=high_impact_size * 2, replace=False
        )
    else:
        selected_high_impact_idx = np.random.choice(
            high_impact_indices, size=high_impact_size * 2, replace=True
        )

    # 组合数据
    normal_x = X[batch_idx]
    normal_y = y[batch_idx]

    high_impact_x = high_impact_data["X"][selected_high_impact_idx]
    high_impact_y = high_impact_data["y"][selected_high_impact_idx]

    # 维度一致性检查，若不一致则回退到普通按标签采样
    if normal_x.shape[1] != high_impact_x.shape[1]:
        print(
            "[warn] Feature dim mismatch between normal and high-impact samples; fallback to normal label-wise sampling."
        )
        return sample_batch_sen_idx_y(X, A, y, batch_size, s)

    batch_x = np.concatenate([normal_x, high_impact_x], axis=0)
    batch_y = np.concatenate([normal_y, high_impact_y], axis=0)

    batch_x = torch.tensor(batch_x).to(device).float()
    batch_y = torch.tensor(batch_y).to(device).float()

    return batch_x, batch_y


def train_dp(
    model,
    criterion,
    optimizer,
    X_train,
    A_train,
    y_train,
    method,
    lam,
    batch_size=500,
    niter=100,
    use_high_impact=False,
    high_impact_ratio=0.04,
    seed=0,
    dataset="adult",
    mode="dp",
    test_k=1,
    high_impact_strategy="default",
    pair_count_K=200,
    gamma_samples=10,
):
    model.train()

    # 加载高影响样本数据
    high_impact_data = None
    if use_high_impact:
        suffix = ""
        if dataset == "synthetic":
            k_value = float(test_k)
            k_formatted = f"{k_value:.2f}"
            k_formatted = k_formatted.rstrip("0").rstrip(".")
            suffix = f"_k{k_formatted}"
        strategy_suffix = (
            f"_{high_impact_strategy}" if high_impact_strategy != "default" else ""
        )
        # 添加超参数后缀，与 fliprate.py 保持一致
        hyperparam_suffix = f"_K{pair_count_K}_g{gamma_samples}"
        data_filepath = f"Adult/all_sample_data_seed{seed}_{mode}_{dataset}{suffix}{strategy_suffix}{hyperparam_suffix}.pkl"
        high_impact_data = load_and_select_high_impact_samples(
            data_filepath, top_k=1000
        )
        # 预先校验维度是否与当前特征一致，否则忽略缓存数据
        high_impact_data = ensure_high_impact_feature_dim(
            high_impact_data, expected_dim=X_train.shape[1]
        )
        if high_impact_data is not None:
            print(f"Using high impact samples with ratio {high_impact_ratio}")
        else:
            print("High impact samples not found, using normal training")

    for it in range(niter):

        # Gender Split
        if use_high_impact and high_impact_data is not None:
            batch_x_0, batch_y_0 = sample_batch_with_high_impact(
                X_train,
                A_train,
                y_train,
                batch_size,
                0,
                high_impact_data,
                high_impact_ratio,
            )
            batch_x_1, batch_y_1 = sample_batch_with_high_impact(
                X_train,
                A_train,
                y_train,
                batch_size,
                1,
                high_impact_data,
                high_impact_ratio,
            )
        else:
            batch_x_0, batch_y_0 = sample_batch_sen_idx(
                X_train, A_train, y_train, batch_size, 0
            )
            batch_x_1, batch_y_1 = sample_batch_sen_idx(
                X_train, A_train, y_train, batch_size, 1
            )

        if method == "mixup":
            # Fair Mixup
            alpha = 1
            gamma = beta(alpha, alpha)

            batch_x_mix = batch_x_0 * gamma + batch_x_1 * (1 - gamma)
            batch_x_mix = batch_x_mix.requires_grad_(True)

            output = model(batch_x_mix)

            # gradient regularization
            gradx = torch.autograd.grad(output.sum(), batch_x_mix, create_graph=True)[0]

            batch_x_d = batch_x_1 - batch_x_0
            grad_inn = (gradx * batch_x_d).sum(1)
            E_grad = grad_inn.mean(0)
            loss_reg = torch.abs(E_grad)

        elif method == "GapReg":
            # Gap Regularizatioon
            output_0 = model(batch_x_0)
            output_1 = model(batch_x_1)
            loss_reg = torch.abs(output_0.mean() - output_1.mean())
        else:
            # ERM
            loss_reg = 0

        # ERM loss
        batch_x = torch.cat((batch_x_0, batch_x_1), 0)
        batch_y = torch.cat((batch_y_0, batch_y_1), 0)

        output = model(batch_x)
        # 确保输出和标签形状匹配
        if output.dim() > 1 and output.size(1) == 1:
            output = output.squeeze(-1)
        if batch_y.dim() > 1 and batch_y.size(1) == 1:
            batch_y = batch_y.squeeze(-1)
        loss_sup = criterion(output, batch_y)

        # final loss
        loss = loss_sup + lam * loss_reg

        optimizer.zero_grad()
        loss.backward()
        optimizer.step()


def train_eo(
    model,
    criterion,
    optimizer,
    X_train,
    A_train,
    y_train,
    method,
    lam,
    batch_size=500,
    niter=100,
    use_high_impact=False,
    high_impact_ratio=0.04,
    seed=0,
    dataset="adult",
    mode="eo",
    test_k=1,
    high_impact_strategy="default",
    verbose=False,
    pair_count_K=200,
    gamma_samples=10,
):
    model.train()

    # 加载高影响样本数据
    high_impact_data = None
    if use_high_impact:
        suffix = ""
        if dataset == "synthetic":
            k_value = float(test_k)
            k_formatted = f"{k_value:.2f}"
            k_formatted = k_formatted.rstrip("0").rstrip(".")
            suffix = f"_k{k_formatted}"
        strategy_suffix = (
            f"_{high_impact_strategy}" if high_impact_strategy != "default" else ""
        )
        # 添加超参数后缀，与 fliprate.py 保持一致
        hyperparam_suffix = f"_K{pair_count_K}_g{gamma_samples}"
        data_filepath = f"Adult/all_sample_data_seed{seed}_{mode}_{dataset}{suffix}{strategy_suffix}{hyperparam_suffix}.pkl"
        high_impact_data = load_and_select_high_impact_samples(
            data_filepath, top_k=1000
        )
        # 预先校验维度是否与当前特征一致，否则忽略缓存数据
        high_impact_data = ensure_high_impact_feature_dim(
            high_impact_data, expected_dim=X_train.shape[1]
        )
        if high_impact_data is not None:
            print(f"Using high impact samples with ratio {high_impact_ratio}")
        else:
            print(f"High impact samples {data_filepath} not found, using normal training")

    for it in range(niter):

        # Gender Split
        if use_high_impact and high_impact_data is not None:
            batch_x_0, batch_y_0 = sample_batch_sen_idx_y_with_high_impact(
                X_train,
                A_train,
                y_train,
                batch_size,
                0,
                high_impact_data,
                high_impact_ratio,
            )
            batch_x_1, batch_y_1 = sample_batch_sen_idx_y_with_high_impact(
                X_train,
                A_train,
                y_train,
                batch_size,
                1,
                high_impact_data,
                high_impact_ratio,
            )
        else:
            batch_x_0, batch_y_0 = sample_batch_sen_idx_y(
                X_train, A_train, y_train, batch_size, 0
            )
            batch_x_1, batch_y_1 = sample_batch_sen_idx_y(
                X_train, A_train, y_train, batch_size, 1
            )

        # separate class
        batch_x_0_ = [batch_x_0[:batch_size], batch_x_0[batch_size:]]
        batch_x_1_ = [batch_x_1[:batch_size], batch_x_1[batch_size:]]

        if method == "mixup":
            loss_reg = 0
            alpha = 1
            for i in range(2):
                gamma = beta(alpha, alpha)
                batch_x_0_i = batch_x_0_[i]
                batch_x_1_i = batch_x_1_[i]

                batch_x_mix = batch_x_0_i * gamma + batch_x_1_i * (1 - gamma)
                batch_x_mix = batch_x_mix.requires_grad_(True)
                output = model(batch_x_mix)

                # gradient regularization
                gradx = torch.autograd.grad(
                    output.sum(), batch_x_mix, create_graph=True
                )[0]
                batch_x_d = batch_x_1_i - batch_x_0_i
                grad_inn = (gradx * batch_x_d).sum(1)
                loss_reg += torch.abs(grad_inn.mean())

        elif method == "GapReg":
            loss_reg = 0
            for i in range(2):
                batch_x_0_i = batch_x_0_[i]
                batch_x_1_i = batch_x_1_[i]

                output_0 = model(batch_x_0_i)
                output_1 = model(batch_x_1_i)
                loss_reg += torch.abs(output_0.mean() - output_1.mean())
        else:
            # ERM
            loss_reg = 0

        # ERM loss
        batch_x = torch.cat((batch_x_0, batch_x_1), 0)
        batch_y = torch.cat((batch_y_0, batch_y_1), 0)

        output = model(batch_x)
        # 确保输出和标签形状匹配
        if output.dim() > 1 and output.size(1) == 1:
            output = output.squeeze(-1)
        if batch_y.dim() > 1 and batch_y.size(1) == 1:
            batch_y = batch_y.squeeze(-1)
        loss_sup = criterion(output, batch_y)

        # final loss
        loss = loss_sup + lam * loss_reg

        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

        if verbose:
            loss_sup_val = loss_sup.detach().item()
            loss_total_val = loss.detach().item()
            if torch.is_tensor(loss_reg):
                loss_reg_val = loss_reg.detach().item()
            else:
                loss_reg_val = float(loss_reg)
            print(
                f"[train_eo][{it + 1}/{niter}] loss_sup={loss_sup_val:.6f} "
                f"loss_reg={loss_reg_val:.6f} total_loss={loss_total_val:.6f}"
            )


def evaluate_dp(model, X_test, y_test, A_test):
    model.eval()
    with torch.no_grad():
        device_model = next(model.parameters()).device
        X_test_t = torch.from_numpy(X_test.astype(np.float32)).to(device_model)
        outputs = model(X_test_t)
        if outputs.dim() > 1 and outputs.size(1) == 1:
            outputs = outputs.squeeze(-1)
        y_pred_probs = outputs.detach().cpu().numpy().ravel()
    y_pred_bin = (y_pred_probs >= 0.5).astype(int)
    # standard implementation
    dp_gap = demographic_parity_difference(
        y_true=y_test, y_pred=y_pred_bin, sensitive_features=A_test
    )
    ap = average_precision_score(y_test, y_pred_probs)
    return ap, abs(dp_gap)


def evaluate_eo(model, X_test, y_test, A_test):
    model.eval()
    with torch.no_grad():
        device_model = next(model.parameters()).device
        X_test_t = torch.from_numpy(X_test.astype(np.float32)).to(device_model)
        outputs = model(X_test_t)
        if outputs.dim() > 1 and outputs.size(1) == 1:
            outputs = outputs.squeeze(-1)
        y_pred_probs = outputs.detach().cpu().numpy().ravel()
    y_pred_bin = (y_pred_probs >= 0.5).astype(int)
    # standard implementation
    eo_gap = equalized_odds_difference(
        y_true=y_test, y_pred=y_pred_bin, sensitive_features=A_test
    )
    eo_gap_mean = equalized_odds_difference(
        y_true=y_test, y_pred=y_pred_bin, sensitive_features=A_test, agg="mean"
    )
    ap = average_precision_score(y_test, y_pred_probs)

    my_metrics = {
        "tpr": recall_score,
        "fpr": false_positive_rate,
        "sel": selection_rate,
        "count": count,
    }
    # Construct a MetricFrame
    mf = MetricFrame(
        metrics=my_metrics, y_true=y_test, y_pred=y_pred_bin, sensitive_features=A_test
    )
    return ap, abs(eo_gap), mf, eo_gap_mean


# def evaluate_dp(model, X_test, y_test, A_test):
#     model.eval()

#     # calculate DP gap
#     idx_0 = np.where(A_test == 0)[0]
#     idx_1 = np.where(A_test == 1)[0]

#     X_test_0 = X_test[idx_0]
#     X_test_1 = X_test[idx_1]
#     X_test_0 = torch.tensor(X_test_0).to(device).float()
#     X_test_1 = torch.tensor(X_test_1).to(device).float()

#     pred_0 = model(X_test_0)
#     pred_1 = model(X_test_1)

#     gap = pred_0.mean() - pred_1.mean()
#     gap = abs(gap.data.cpu().numpy())

#     # calculate average precision
#     X_test_device = torch.tensor(X_test).to(device).float()
#     output = model(X_test_device)
#     output_flat = output
#     if output_flat.dim() > 1 and output_flat.size(1) == 1:
#         output_flat = output_flat.squeeze(-1)
#     y_scores = output_flat.data.cpu().numpy()
#     ap = average_precision_score(y_test, y_scores)

#     return ap, gap


# def evaluate_eo(model, X_test, y_test, A_test):
#     model.eval()
#     idx_00 = list(set(np.where(A_test == 0)[0]) & set(np.where(y_test == 0)[0]))
#     idx_01 = list(set(np.where(A_test == 0)[0]) & set(np.where(y_test == 1)[0]))
#     idx_10 = list(set(np.where(A_test == 1)[0]) & set(np.where(y_test == 0)[0]))
#     idx_11 = list(set(np.where(A_test == 1)[0]) & set(np.where(y_test == 1)[0]))

#     X_test_00 = X_test[idx_00]
#     X_test_01 = X_test[idx_01]
#     X_test_10 = X_test[idx_10]
#     X_test_11 = X_test[idx_11]

#     X_test_00 = torch.tensor(X_test_00).to(device).float()
#     X_test_01 = torch.tensor(X_test_01).to(device).float()
#     X_test_10 = torch.tensor(X_test_10).to(device).float()
#     X_test_11 = torch.tensor(X_test_11).to(device).float()

#     pred_00 = model(X_test_00)
#     pred_01 = model(X_test_01)
#     pred_10 = model(X_test_10)
#     pred_11 = model(X_test_11)

#     gap_0 = pred_00.mean() - pred_10.mean()
#     gap_1 = pred_01.mean() - pred_11.mean()
#     gap_0 = abs(gap_0.data.cpu().numpy())
#     gap_1 = abs(gap_1.data.cpu().numpy())

#     gap = gap_0 + gap_1

#     # calculate average precision
#     X_test_device = torch.tensor(X_test).to(device).float()
#     output = model(X_test_device)
#     output_flat = output
#     if output_flat.dim() > 1 and output_flat.size(1) == 1:
#         output_flat = output_flat.squeeze(-1)
#     y_scores = output_flat.data.cpu().numpy()
#     ap = average_precision_score(y_test, y_scores)

#     return ap, gap
