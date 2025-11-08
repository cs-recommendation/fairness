import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from numpy.random import beta
from tqdm import tqdm
import pickle
import os

from dataset import preprocess_adult_data
from model import Net


G = 100  # 默认的全局gamma采样次数（用于向后兼容）


def train_base_model(X_train, y_train, input_size, num_epochs=100, batch_size=500):
    """
    训练一个不含reg_loss的基础模型m0
    """
    model = Net(input_size=input_size).cuda()
    optimizer = optim.Adam(model.parameters(), lr=1e-3)
    criterion = nn.BCELoss()

    model.train()
    n_samples = len(X_train)

    for epoch in tqdm(range(num_epochs), desc="Training base model"):
        # 随机采样batch
        batch_indices = np.random.choice(n_samples, size=batch_size, replace=False)
        batch_x = torch.tensor(X_train[batch_indices]).cuda().float()
        batch_y = torch.tensor(y_train[batch_indices]).cuda().float()

        # 前向传播
        output = model(batch_x)
        # 确保输出和标签形状匹配
        if output.dim() > 1 and output.size(1) == 1:
            output = output.squeeze(-1)
        if batch_y.dim() > 1 and batch_y.size(1) == 1:
            batch_y = batch_y.squeeze(-1)
        loss = criterion(output, batch_y)

        # 反向传播
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

    return model


def group_samples_by_sensitive_attribute_and_label(X_train, A_train, y_train):
    """
    根据敏感属性和标签将训练集划分为不同的group (EO模式)
    返回: dict, key为(sensitive_attr, label), value为样本索引列表
    """
    groups = {}
    for i in range(len(X_train)):
        key = (int(A_train[i]), int(y_train[i]))
        if key not in groups:
            groups[key] = []
        groups[key].append(i)

    return groups


def group_samples_by_sensitive_attribute_only(X_train, A_train, y_train):
    """
    只根据敏感属性将训练集划分为不同的group (DP模式)
    返回: dict, key为sensitive_attr, value为样本索引列表
    """
    groups = {}
    for i in range(len(X_train)):
        key = int(A_train[i])
        if key not in groups:
            groups[key] = []
        groups[key].append(i)

    return groups


def calculate_pair_fliprate(model, x1, x2, y1, y2, k=10, gamma_samples=None):
    """
    计算一对样本的fliprate

    Args:
        model: 训练好的基础模型
        x1, x2: 两个样本
        y1, y2: 对应的标签
        k: gamma采样次数（已弃用，使用 gamma_samples）
        gamma_samples: gamma采样次数（新参数，优先使用）

    Returns:
        fliprate: 预测值不等于原标签的比例
    """
    # 优先使用 gamma_samples，如果未提供则使用 k
    if gamma_samples is not None:
        k = gamma_samples
    model.eval()
    flip_count = 0

    with torch.no_grad():
        for _ in range(k):
            # 随机生成gamma
            alpha = 1.0
            gamma = beta(alpha, alpha)

            # 生成混合样本
            x_mix = x1 * gamma + x2 * (1 - gamma)
            x_mix = x_mix.unsqueeze(0)  # 添加batch维度

            # 预测
            pred = model(x_mix)
            pred_label = (pred > 0.5).float().item()

            # 检查是否flip（这里我们检查是否与y1或y2不同）
            # 按照需求，我们计算预测值不等于原标签的情况
            if gamma > 0.5:
                original_label = y2
            else:
                original_label = y1

            if pred_label != original_label:
                flip_count += 1

    return flip_count / k


def calculate_sample_fliprates_eo(
    model, groups, X_train, y_train, K=200, gamma_samples=10
):
    """
    计算每个样本的平均fliprate (EO模式)

    Args:
        model: 训练好的基础模型
        groups: 按敏感属性和标签分组的样本
        X_train, y_train: 训练数据
        K: 每个样本选择的pair数量
        gamma_samples: gamma采样次数

    Returns:
        sample_fliprates: dict, key为样本索引，value为该样本的平均fliprate
    """
    import time

    start_time = time.time()

    sample_fliprates = {}

    # 为每个样本初始化fliprate列表
    all_indices = []
    for indices in groups.values():
        all_indices.extend(indices)
    for idx in all_indices:
        sample_fliprates[idx] = []

    print(f"总共需要处理 {len(all_indices)} 个样本")

    total_pairs_calculated = 0
    total_samples_processed = 0

    # 对每个group中的样本，寻找敏感属性不同但y值相同的其他group中的样本进行配对
    for group_idx, (group_key, indices) in enumerate(groups.items()):
        sensitive_attr, label = group_key
        group_start_time = time.time()
        print(
            f"\n[{group_idx+1}/{len(groups)}] Processing group {group_key} with {len(indices)} samples"
        )

        # 寻找敏感属性不同但y值相同的其他groups
        target_groups = []
        for other_key, other_indices in groups.items():
            other_sensitive_attr, other_label = other_key
            # 敏感属性不同但y值相同
            if other_sensitive_attr != sensitive_attr and other_label == label:
                target_groups.extend(other_indices)

        if not target_groups:
            print(
                f"  ❌ No target group found for group {group_key} (敏感属性不同但y值相同)"
            )
            continue

        print(f"  ✅ Found {len(target_groups)} samples in target groups for pairing")

        # 对当前group中的每个样本，从target_groups中选择K个样本作为pair
        for sample_idx, idx1 in enumerate(indices):
            sample_start_time = time.time()

            # 如果target_groups样本数量少于K，就用所有target_groups样本
            if len(target_groups) <= K:
                selected_indices = target_groups
            else:
                # 随机选择K个样本
                selected_indices = np.random.choice(
                    target_groups, size=K, replace=False
                ).tolist()

            # 计算与选定样本的fliprate
            pair_count = 0
            for idx2 in selected_indices:
                x1 = torch.tensor(X_train[idx1]).cuda().float()
                x2 = torch.tensor(X_train[idx2]).cuda().float()
                y1 = y_train[idx1]
                y2 = y_train[idx2]

                # 计算这一对的fliprate
                fliprate = calculate_pair_fliprate(
                    model, x1, x2, y1, y2, gamma_samples=gamma_samples
                )

                # 将fliprate添加到idx1的记录中
                sample_fliprates[idx1].append(fliprate)
                pair_count += 1

            total_pairs_calculated += pair_count
            total_samples_processed += 1

            sample_time = time.time() - sample_start_time

            # 每处理10个样本或每个组的最后一个样本时打印进度
            if (sample_idx + 1) % 10 == 0 or sample_idx == len(indices) - 1:
                avg_time_per_sample = (
                    sample_time
                    if sample_idx == 0
                    else (time.time() - group_start_time) / (sample_idx + 1)
                )
                remaining_samples = len(indices) - sample_idx - 1
                eta_group = remaining_samples * avg_time_per_sample

                print(
                    f"    样本进度: [{sample_idx+1}/{len(indices)}], "
                    f"当前样本用时: {sample_time:.2f}s, "
                    f"平均每样本: {avg_time_per_sample:.2f}s, "
                    f"配对数: {pair_count}, "
                    f"组内剩余时间: {eta_group:.1f}s"
                )

        group_time = time.time() - group_start_time
        print(
            f"  组 {group_key} 完成，用时: {group_time:.2f}s, 处理了 {len(indices)} 个样本"
        )

    # 计算每个样本的平均fliprate
    print(f"\n计算平均fliprate...")
    avg_fliprates = {}
    for idx, fliprates in sample_fliprates.items():
        if fliprates:  # 如果有fliprate记录
            avg_fliprates[idx] = np.mean(fliprates)
        else:
            avg_fliprates[idx] = 0.0

    total_time = time.time() - start_time
    print(f"\n=== 性能统计 ===")
    print(f"总用时: {total_time:.2f}s ({total_time/60:.1f}分钟)")
    print(f"处理样本数: {total_samples_processed}")
    print(f"计算配对数: {total_pairs_calculated}")
    print(f"平均每样本用时: {total_time/total_samples_processed:.3f}s")
    print(f"平均每配对用时: {total_time/total_pairs_calculated:.4f}s")
    print(f"每秒处理样本数: {total_samples_processed/total_time:.1f}")
    print(f"每秒计算配对数: {total_pairs_calculated/total_time:.1f}")

    return avg_fliprates


def calculate_pair_fliprate_batch(
    model, x1_batch, x2_batch, y1_batch, y2_batch, k=G, gamma_samples=None
):
    """
    批量计算多对样本的fliprate (优化版本)

    Args:
        model: 训练好的基础模型
        x1_batch, x2_batch: 样本批次 (batch_size, features)
        y1_batch, y2_batch: 对应的标签批次
        k: gamma采样次数（已弃用，使用 gamma_samples）
        gamma_samples: gamma采样次数（新参数，优先使用）

    Returns:
        fliprates: 每对样本的fliprate列表
    """
    # 优先使用 gamma_samples，如果未提供则使用 k
    if gamma_samples is not None:
        k = gamma_samples
    model.eval()
    batch_size = x1_batch.shape[0]
    flip_counts = torch.zeros(batch_size, device=x1_batch.device)

    with torch.no_grad():
        for _ in range(k):
            # 批量生成gamma
            gammas = torch.tensor(
                [beta(1.0, 1.0) for _ in range(batch_size)],
                device=x1_batch.device,
                dtype=x1_batch.dtype,
            ).unsqueeze(1)

            # 批量生成混合样本
            x_mix_batch = x1_batch * gammas + x2_batch * (1 - gammas)

            # 批量预测
            preds = model(x_mix_batch)
            pred_labels = (preds > 0.5).float().squeeze()

            # 批量检查flip
            original_labels = torch.tensor(
                y1_batch, device=x1_batch.device, dtype=torch.float32
            )
            flips = (pred_labels != original_labels).float()
            flip_counts += flips

    return (flip_counts / k).cpu().numpy()


def calculate_sample_fliprates_eo_optimized(
    model, groups, X_train, y_train, K=200, batch_size=32, gamma_samples=10
):
    """
    计算每个样本的平均fliprate (EO模式 - 优化版本)

    Args:
        model: 训练好的基础模型
        groups: 按敏感属性和标签分组的样本
        X_train, y_train: 训练数据
        K: 每个样本选择的pair数量
        batch_size: 批处理大小
        gamma_samples: gamma采样次数

    Returns:
        sample_fliprates: dict, key为样本索引，value为该样本的平均fliprate
    """
    import time

    start_time = time.time()

    sample_fliprates = {}

    # 为每个样本初始化fliprate列表
    all_indices = []
    for indices in groups.values():
        all_indices.extend(indices)
    for idx in all_indices:
        sample_fliprates[idx] = []

    print(
        f"总共需要处理 {len(all_indices)} 个样本 (优化版本，批处理大小: {batch_size})"
    )

    total_pairs_calculated = 0
    total_samples_processed = 0

    # 预先将数据转换为tensor
    X_train_tensor = torch.tensor(X_train).cuda().float()

    # 对每个group中的样本，寻找敏感属性不同但y值相同的其他group中的样本进行配对
    for group_idx, (group_key, indices) in enumerate(groups.items()):
        sensitive_attr, label = group_key
        group_start_time = time.time()
        print(
            f"\n[{group_idx+1}/{len(groups)}] Processing group {group_key} with {len(indices)} samples"
        )

        # 寻找敏感属性不同但y值相同的其他groups
        target_groups = []
        for other_key, other_indices in groups.items():
            other_sensitive_attr, other_label = other_key
            # 敏感属性不同但y值相同
            if other_sensitive_attr != sensitive_attr and other_label == label:
                target_groups.extend(other_indices)

        if not target_groups:
            print(f"  ❌ No target group found for group {group_key}")
            continue

        print(f"  ✅ Found {len(target_groups)} samples in target groups for pairing")

        # 批量处理当前group中的样本
        for i in range(0, len(indices), batch_size):
            batch_indices = indices[i : i + batch_size]
            batch_start_time = time.time()

            # 为当前批次的每个样本收集配对
            batch_pairs = []
            batch_idx1_list = []

            for idx1 in batch_indices:
                # 选择配对样本
                if len(target_groups) <= K:
                    selected_indices = target_groups
                else:
                    selected_indices = np.random.choice(
                        target_groups, size=K, replace=False
                    ).tolist()

                for idx2 in selected_indices:
                    batch_pairs.append((idx1, idx2))
                    batch_idx1_list.append(idx1)

            if batch_pairs:
                # 准备批量数据
                x1_batch = X_train_tensor[[pair[0] for pair in batch_pairs]]
                x2_batch = X_train_tensor[[pair[1] for pair in batch_pairs]]
                y1_batch = [y_train[pair[0]] for pair in batch_pairs]
                y2_batch = [y_train[pair[1]] for pair in batch_pairs]

                # 批量计算fliprate
                fliprates = calculate_pair_fliprate_batch(
                    model,
                    x1_batch,
                    x2_batch,
                    y1_batch,
                    y2_batch,
                    gamma_samples=gamma_samples,
                )

                # 将结果分配给对应的样本
                for j, (idx1, idx2) in enumerate(batch_pairs):
                    sample_fliprates[idx1].append(fliprates[j])

                total_pairs_calculated += len(batch_pairs)

            total_samples_processed += len(batch_indices)
            batch_time = time.time() - batch_start_time

            print(
                f"    批次进度: [{i+len(batch_indices)}/{len(indices)}], "
                f"批次用时: {batch_time:.2f}s, "
                f"配对数: {len(batch_pairs) if batch_pairs else 0}"
            )

        group_time = time.time() - group_start_time
        print(f"  组 {group_key} 完成，用时: {group_time:.2f}s")

    # 计算每个样本的平均fliprate
    print(f"\n计算平均fliprate...")
    avg_fliprates = {}
    for idx, fliprates in sample_fliprates.items():
        if fliprates:
            avg_fliprates[idx] = np.mean(fliprates)
        else:
            avg_fliprates[idx] = 0.0

    total_time = time.time() - start_time
    print(f"\n=== 优化版性能统计 ===")
    print(f"总用时: {total_time:.2f}s ({total_time/60:.1f}分钟)")
    print(f"处理样本数: {total_samples_processed}")
    print(f"计算配对数: {total_pairs_calculated}")
    print(f"平均每样本用时: {total_time/total_samples_processed:.3f}s")
    print(f"平均每配对用时: {total_time/total_pairs_calculated:.4f}s")
    print(f"每秒处理样本数: {total_samples_processed/total_time:.1f}")
    print(f"每秒计算配对数: {total_pairs_calculated/total_time:.1f}")

    return avg_fliprates


def calculate_sample_fliprates_dp(
    model, groups, X_train, y_train, K=2000, gamma_samples=10
):
    """
    计算每个样本的平均fliprate (DP模式)

    Args:
        model: 训练好的基础模型
        groups: 只按敏感属性分组的样本
        X_train, y_train: 训练数据
        K: 每个样本选择的pair数量
        gamma_samples: gamma采样次数

    Returns:
        sample_fliprates: dict, key为样本索引，value为该样本的平均fliprate
    """
    sample_fliprates = {}

    # 为每个样本初始化fliprate列表
    all_indices = []
    for indices in groups.values():
        all_indices.extend(indices)
    for idx in all_indices:
        sample_fliprates[idx] = []

    # 对每个group中的样本，从其他敏感属性组中取样本进行配对
    for group_key, indices in groups.items():
        sensitive_attr = group_key
        print(f"Processing group {group_key} with {len(indices)} samples")

        # 寻找敏感属性不同的其他groups
        target_groups = []
        for other_key, other_indices in groups.items():
            other_sensitive_attr = other_key
            # 敏感属性不同
            if other_sensitive_attr != sensitive_attr:
                target_groups.extend(other_indices)

        if not target_groups:
            print(f"  No target group found for group {group_key} (敏感属性不同)")
            continue

        print(f"  Found {len(target_groups)} samples in target groups for pairing")

        # 对当前group中的每个样本，从target_groups中选择K个样本作为pair
        for idx1 in indices:
            # 如果target_groups样本数量少于K，就用所有target_groups样本
            if len(target_groups) <= K:
                selected_indices = target_groups
            else:
                # 随机选择K个样本
                selected_indices = np.random.choice(
                    target_groups, size=K, replace=False
                ).tolist()

            # 计算与选定样本的fliprate
            for idx2 in selected_indices:
                x1 = torch.tensor(X_train[idx1]).cuda().float()
                x2 = torch.tensor(X_train[idx2]).cuda().float()
                y1 = y_train[idx1]
                y2 = y_train[idx2]

                # 计算这一对的fliprate
                fliprate = calculate_pair_fliprate(
                    model, x1, x2, y1, y2, gamma_samples=gamma_samples
                )

                # 将fliprate添加到idx1的记录中
                sample_fliprates[idx1].append(fliprate)

    # 计算每个样本的平均fliprate
    avg_fliprates = {}
    for idx, fliprates in sample_fliprates.items():
        if fliprates:  # 如果有fliprate记录
            avg_fliprates[idx] = np.mean(fliprates)
        else:
            avg_fliprates[idx] = 0.0

    return avg_fliprates


def calculate_sample_fliprates(
    model,
    groups,
    X_train,
    y_train,
    K=200,
    mode="eo",
    optimized=True,
    batch_size=32,
    gamma_samples=10,
):
    """
    计算每个样本的平均fliprate (统一接口)

    Args:
        model: 训练好的基础模型
        groups: 分组的样本
        X_train, y_train: 训练数据
        K: 每个样本选择的pair数量
        mode: 'eo' 或 'dp'
        optimized: 是否使用优化版本 (仅支持EO模式)
        batch_size: 批处理大小 (仅优化版本使用)
        gamma_samples: gamma采样次数

    Returns:
        sample_fliprates: dict, key为样本索引，value为该样本的平均fliprate
    """
    if mode == "eo":
        if optimized:
            return calculate_sample_fliprates_eo_optimized(
                model, groups, X_train, y_train, K, batch_size, gamma_samples
            )
        else:
            return calculate_sample_fliprates_eo(
                model, groups, X_train, y_train, K, gamma_samples
            )
    elif mode == "dp":
        return calculate_sample_fliprates_dp(
            model, groups, X_train, y_train, K, gamma_samples
        )
    else:
        raise ValueError(f"Unsupported mode: {mode}. Use 'eo' or 'dp'.")


def save_all_sample_data(
    sample_fliprates, X_train, y_train, A_train, filepath="all_sample_data.pkl"
):
    """
    保存所有样本的数据和fliprate

    Args:
        sample_fliprates: 每个样本的fliprate字典
        X_train, y_train, A_train: 训练数据
        filepath: 保存路径
    """
    all_sample_data = {
        "sample_fliprates": sample_fliprates,
        "X_train": X_train,
        "y_train": y_train,
        "A_train": A_train,
    }

    with open(filepath, "wb") as f:
        pickle.dump(all_sample_data, f)
    print(f"All sample data saved to {filepath}")


def load_and_select_high_impact_samples(
    filepath,
    top_k=1000,
):
    """
    从文件加载所有样本数据，并选择top_k个高影响样本

    Args:
        filepath: 数据文件路径
        top_k: 选择的高影响样本数量

    Returns:
        high_impact_data: 高影响样本数据，如果文件不存在或损坏返回None
    """
    if not os.path.exists(filepath):
        return None

    try:
        with open(filepath, "rb") as f:
            all_data = pickle.load(f)
    except (EOFError, pickle.UnpicklingError, Exception) as e:
        print(f"警告: 文件 {filepath} 损坏或读取失败: {e}")
        print(f"删除损坏的文件并将重新计算...")
        try:
            os.remove(filepath)
        except:
            pass
        return None

    sample_fliprates = all_data["sample_fliprates"]
    X_train = all_data["X_train"]
    y_train = all_data["y_train"]
    A_train = all_data["A_train"]

    # 按fliprate排序并选择top_k个样本
    sorted_samples = sorted(sample_fliprates.items(), key=lambda x: x[1], reverse=True)
    high_impact_indices = [idx for idx, _ in sorted_samples[:top_k]]

    # 准备高影响样本数据
    high_impact_data = {
        "indices": high_impact_indices,
        "X": X_train[high_impact_indices],
        "y": y_train[high_impact_indices],
        "A": A_train[high_impact_indices],
        "fliprates": [sample_fliprates[idx] for idx in high_impact_indices],
    }

    return high_impact_data


def ensure_high_impact_feature_dim(high_impact_data, expected_dim):
    """
    若高影响样本维度与当前训练特征维度不一致，返回None以触发回退/重算。
    """
    if high_impact_data is None:
        return None
    X_hi = high_impact_data.get("X", None)
    if X_hi is None or X_hi.ndim != 2:
        return None
    if X_hi.shape[1] != expected_dim:
        print(
            f"[warn] High-impact feature dim {X_hi.shape[1]} != expected {expected_dim}. Ignoring cached data."
        )
        return None

    return high_impact_data


def load_high_impact_samples(filepath="high_impact_samples.pkl", top_k=1000):
    """
    从文件加载高影响样本（兼容旧接口）

    Args:
        filepath: 文件路径（会自动转换为all_sample_data格式）
        top_k: 选择的高影响样本数量

    Returns:
        high_impact_data: 高影响样本数据
    """
    # 将旧的文件路径格式转换为新的格式
    if "high_impact_samples_seed" in filepath:
        # 提取seed信息
        parts = filepath.split("_")
        seed_part = [p for p in parts if p.startswith("seed")][0]
        seed = seed_part.replace("seed", "").replace(".pkl", "")
        new_filepath = f"all_sample_data_seed{seed}.pkl"
    else:
        new_filepath = filepath.replace("high_impact_samples", "all_sample_data")

    return load_and_select_high_impact_samples(new_filepath, top_k)


def preprocess_high_impact_samples(
    seed=0,
    top_k=1000,
    force_recompute=False,
    dataset="adult",
    mode="dp",
    synthetic_test_k=1,
    apply_fairshift=False,
    high_impact_strategy="default",
    K=200,
    gamma_samples=10,
):
    """
    完整的高影响样本预处理流程

    Args:
        seed: 随机种子
        top_k: 选择的高影响样本数量
        force_recompute: 是否强制重新计算（忽略已保存的结果）
        dataset: 数据集名称 ('adult', 'interpolation', 'synthetic')
        mode: 'eo' (Equalized Opportunity) 或 'dp' (Demographic Parity)
        synthetic_test_k: synthetic数据集的k值
        apply_fairshift: 是否应用fairshift预处理
        high_impact_strategy: 高影响样本策略
        K: 每个样本选择的pair数量
        gamma_samples: gamma采样次数

    Returns:
        high_impact_data: 高影响样本数据
    """
    synthetic_suffix = ""
    if dataset == "synthetic":
        k_value = float(synthetic_test_k)
        k_formatted = (
            f"{k_value:.2f}"
            if not np.isclose(k_value, round(k_value))
            else str(int(round(k_value)))
        )
        k_formatted = k_formatted.rstrip("0").rstrip(".")
        synthetic_suffix = f"_k{k_formatted}"

    strategy_suffix = (
        f"_{high_impact_strategy}" if high_impact_strategy != "default" else ""
    )

    # 包含 K 和 gamma_samples 参数以避免不同超参组合共享缓存
    hyperparam_suffix = f"_K{K}_g{gamma_samples}"

    data_filepath = f"tmp/fliprate_highimpact_sample/all_sample_data_seed{seed}_{mode}_{dataset}{synthetic_suffix}{strategy_suffix}{hyperparam_suffix}.pkl"

    # 检查是否已经存在计算结果
    if not force_recompute:
        existing_data = load_and_select_high_impact_samples(data_filepath, top_k)
        if existing_data is not None:
            print(
                f"Loading existing sample data from {data_filepath} and selecting top {top_k}"
            )
            print(f"Selected {len(existing_data['indices'])} high impact samples.")
            print(
                f"Average fliprate of selected samples: {np.mean(existing_data['fliprates']):.4f}"
            )
            return existing_data

    print(f"Computing sample fliprates in {mode.upper()} mode...")

    # 1. 获取训练数据
    if dataset == "adult":
        X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
            preprocess_adult_data(seed=seed)
        )
    elif dataset == "interpolation":
        from dataset import preprocess_interpolation_data

        X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
            preprocess_interpolation_data(seed=seed)
        )
    elif dataset == "synthetic":
        from dataset import preprocess_synthetic_data

        X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
            preprocess_synthetic_data(
                seed=seed,
                test_k=synthetic_test_k,
            )
        )
    else:
        raise ValueError(f"Unsupported dataset: {dataset}")

    if apply_fairshift:
        from fairshift import align_train_with_target

        align_seed = align_seed if align_seed is not None else seed
        X_train, y_train, A_train = align_train_with_target(
            X_train,
            y_train,
            A_train,
            y_test,
            A_test,
            random_state=align_seed,
        )

    # 2. 训练基础模型m0
    print("Training base model m0...")
    base_model = train_base_model(X_train, y_train, input_size=len(X_train[0]))

    # 3. 根据模式分组
    print("Grouping samples...")
    if mode == "eo":
        groups = group_samples_by_sensitive_attribute_and_label(
            X_train, A_train, y_train
        )
        print(f"Created {len(groups)} groups (EO mode):")
        for key, indices in groups.items():
            print(f"  Group (A={key[0]}, y={key[1]}): {len(indices)} samples")
    elif mode == "dp":
        groups = group_samples_by_sensitive_attribute_only(X_train, A_train, y_train)
        print(f"Created {len(groups)} groups (DP mode):")
        for key, indices in groups.items():
            print(f"  Group (A={key}): {len(indices)} samples")
    else:
        raise ValueError(f"Unsupported mode: {mode}. Use 'eo' or 'dp'.")

    # 4. 计算每个样本的fliprate
    print(
        f"Calculating fliprates with K={K} pairs per sample, gamma_samples={gamma_samples} in {mode.upper()} mode..."
    )
    sample_fliprates = calculate_sample_fliprates(
        base_model,
        groups,
        X_train,
        y_train,
        K=K,
        mode=mode,
        gamma_samples=gamma_samples,
    )

    # 5. 保存所有样本数据
    print("Saving all sample data...")
    # 确保目录存在
    os.makedirs(os.path.dirname(data_filepath), exist_ok=True)
    save_all_sample_data(sample_fliprates, X_train, y_train, A_train, data_filepath)

    # 6. 选择并返回高影响样本
    print(f"Selecting top {top_k} high impact samples...")
    high_impact_data = load_and_select_high_impact_samples(data_filepath, top_k)

    print(
        f"Preprocessing completed. Selected {len(high_impact_data['indices'])} high impact samples."
    )
    print(
        f"Average fliprate of selected samples: {np.mean(high_impact_data['fliprates']):.4f}"
    )

    return high_impact_data


if __name__ == "__main__":
    # 测试预处理流程
    print("=== Testing EO mode ===")
    high_impact_data_eo = preprocess_high_impact_samples(
        seed=0, top_k=1000, K=200, mode="eo"
    )

    print("\n=== Testing DP mode ===")
    high_impact_data_dp = preprocess_high_impact_samples(
        seed=0, top_k=1000, K=200, mode="dp"
    )
