import pdb
import numpy as np
import pandas as pd
import torch
from aif360.datasets import BinaryLabelDataset
from sklearn.preprocessing import OneHotEncoder, StandardScaler
import os
import matplotlib.pyplot as plt


def _quantization_binning(data, num_bins=10):
    qtls = np.arange(0.0, 1.0 + 1 / num_bins, 1 / num_bins)
    bin_edges = np.quantile(data, qtls, axis=0)  # (num_bins + 1, num_features)
    bin_widths = np.diff(bin_edges, axis=0)
    bin_centers = bin_edges[:-1] + bin_widths / 2  # ()
    return bin_edges, bin_centers, bin_widths


def _quantize(inputs, bin_edges, num_bins=10):
    quant_inputs = np.zeros(inputs.shape[0])
    for i, x in enumerate(inputs):
        quant_inputs[i] = np.digitize(x, bin_edges)
    quant_inputs = quant_inputs.clip(1, num_bins) - 1  # Clip edges
    return quant_inputs


def _one_hot(a, num_bins=10):
    return np.squeeze(np.eye(num_bins)[a.reshape(-1).astype(np.int32)])


def DataQuantize(X, bin_edges=None, num_bins=10):
    """
    Quantize: First 4 entries are continuos, and the rest are binary
    """
    X_ = []
    for i in range(5):
        if bin_edges is not None:
            Xi_q = _quantize(X[:, i], bin_edges, num_bins)
        else:
            bin_edges, bin_centers, bin_widths = _quantization_binning(
                X[:, i], num_bins
            )
            Xi_q = _quantize(X[:, i], bin_edges, num_bins)
        Xi_q = _one_hot(Xi_q, num_bins)
        X_.append(Xi_q)

    for i in range(5, len(X[0])):
        if i == 39:  # gender attribute
            continue
        Xi_q = _one_hot(X[:, i], num_bins=2)
        X_.append(Xi_q)

    return np.concatenate(X_, 1), bin_edges


def get_adult_data():
    """
    We borrow the code from https://github.com/IBM/sensitive-subspace-robustness
    Preprocess the adult data set by removing some features and put adult data into a BinaryLabelDataset
    You need to download the adult dataset (both the adult.data and adult.test files) from https://archive.ics.uci.edu/ml/datasets/Adult
    """

    headers = [
        "age",
        "workclass",
        "fnlwgt",
        "education",
        "education-num",
        "marital-stataus",
        "occupation",
        "relationship",
        "race",
        "sex",
        "capital-gain",
        "capital-loss",
        "hours-per-week",
        "native-country",
        "y",
    ]

    train = pd.read_csv("adult/adult.data", header=None)
    test = pd.read_csv("adult/adult.test", header=None, skiprows=1)
    df = pd.concat([train, test], ignore_index=True)
    df.columns = headers

    df["y"] = (
        df["y"]
        .replace({" <=50K.": 0, " >50K.": 1, " >50K": 1, " <=50K": 0})
        .infer_objects(copy=False)
    )

    df = df.drop(df[(df[headers[-2]] == " ?") | (df[headers[6]] == " ?")].index)
    df = pd.get_dummies(
        df,
        columns=[
            headers[1],
            headers[5],
            headers[6],
            headers[7],
            headers[9],
            headers[8],
            "native-country",
        ],
    )

    delete_these = [
        "race_ Amer-Indian-Eskimo",
        "race_ Asian-Pac-Islander",
        "race_ Black",
        "race_ Other",
        "sex_ Female",
    ]

    delete_these += [
        "native-country_ Cambodia",
        "native-country_ Canada",
        "native-country_ China",
        "native-country_ Columbia",
        "native-country_ Cuba",
        "native-country_ Dominican-Republic",
        "native-country_ Ecuador",
        "native-country_ El-Salvador",
        "native-country_ England",
        "native-country_ France",
        "native-country_ Germany",
        "native-country_ Greece",
        "native-country_ Guatemala",
        "native-country_ Haiti",
        "native-country_ Holand-Netherlands",
        "native-country_ Honduras",
        "native-country_ Hong",
        "native-country_ Hungary",
        "native-country_ India",
        "native-country_ Iran",
        "native-country_ Ireland",
        "native-country_ Italy",
        "native-country_ Jamaica",
        "native-country_ Japan",
        "native-country_ Laos",
        "native-country_ Mexico",
        "native-country_ Nicaragua",
        "native-country_ Outlying-US(Guam-USVI-etc)",
        "native-country_ Peru",
        "native-country_ Philippines",
        "native-country_ Poland",
        "native-country_ Portugal",
        "native-country_ Puerto-Rico",
        "native-country_ Scotland",
        "native-country_ South",
        "native-country_ Taiwan",
        "native-country_ Thailand",
        "native-country_ Trinadad&Tobago",
        "native-country_ United-States",
        "native-country_ Vietnam",
        "native-country_ Yugoslavia",
    ]

    delete_these += ["fnlwgt", "education"]

    df.drop(delete_these, axis=1, inplace=True)

    return BinaryLabelDataset(
        df=df, label_names=["y"], protected_attribute_names=["sex_ Male", "race_ White"]
    )


def DataQuantizeInterpolation(X, bin_edges=None, num_bins=10):
    """
    为 interpolation 数据集进行量化处理
    对连续特征进行分箱量化，然后进行 one-hot 编码

    Args:
        X: 输入特征矩阵 (n_samples, n_features)
        bin_edges: 预计算的分箱边界，如果为None则重新计算
        num_bins: 分箱数量

    Returns:
        X_quantized: 量化后的特征矩阵
        bin_edges: 分箱边界
    """
    X_ = []
    n_features = X.shape[1]

    # 初始化 bin_edges 列表
    if bin_edges is None:
        bin_edges = []

    # 对所有连续特征进行量化处理
    for i in range(n_features):
        if i < len(bin_edges) and bin_edges[i] is not None:
            # 使用预计算的分箱边界
            Xi_q = _quantize(X[:, i], bin_edges[i], num_bins)
        else:
            # 计算新的分箱边界
            bin_edges_i, bin_centers, bin_widths = _quantization_binning(
                X[:, i], num_bins
            )
            Xi_q = _quantize(X[:, i], bin_edges_i, num_bins)
            # 确保 bin_edges 列表有足够的元素
            while len(bin_edges) <= i:
                bin_edges.append(None)
            bin_edges[i] = bin_edges_i

        Xi_q = _one_hot(Xi_q, num_bins)
        X_.append(Xi_q)

    return np.concatenate(X_, 1), bin_edges


def preprocess_interpolation_data(seed=0, use_embedding=False, num_bins=10):
    """
    处理 interpolation 数据集

    Args:
        seed: 随机种子
        use_embedding: 是否使用 embedding (量化处理)
        num_bins: 分箱数量，仅在 use_embedding=True 时使用

    Returns:
        X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test
    """
    # 读取训练和测试数据
    train_data = pd.read_csv("interpolation_8_30000/train_full.csv")
    test_data = pd.read_csv("interpolation_8_30000/test_full.csv")

    # 提取特征、敏感属性和标签
    # 前8列是特征 (X1_transformed 到 X8_transformed)
    feature_columns = [f"X{i}_transformed" for i in range(1, 9)]
    X_train = train_data[feature_columns].values
    X_test = test_data[feature_columns].values

    # A 是敏感属性
    A_train = train_data["A"].values
    A_test = test_data["A"].values

    # y 是标签
    y_train = train_data["y"].values.astype(np.float32)
    y_test = test_data["y"].values.astype(np.float32)

    # 从训练集中分出验证集
    np.random.seed(seed)
    val_size = len(X_test)
    val_indices = np.random.choice(len(X_train), val_size, replace=False)
    train_indices = np.setdiff1d(range(len(X_train)), val_indices)

    X_val = X_train[val_indices]
    y_val = y_train[val_indices]
    A_val = A_train[val_indices]

    X_train = X_train[train_indices]
    y_train = y_train[train_indices]
    A_train = A_train[train_indices]

    if use_embedding:
        # 使用量化处理进行 embedding
        X_train, bin_edges = DataQuantizeInterpolation(X_train, num_bins=num_bins)
        X_val, _ = DataQuantizeInterpolation(X_val, bin_edges, num_bins=num_bins)
        X_test, _ = DataQuantizeInterpolation(X_test, bin_edges, num_bins=num_bins)
    else:
        # 标准化特征（原始处理方式）
        from sklearn.preprocessing import StandardScaler

        scaler = StandardScaler()
        X_train = scaler.fit_transform(X_train)
        X_val = scaler.transform(X_val)
        X_test = scaler.transform(X_test)

    return X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test


def preprocess_adult_data(seed=0):
    """
    Description: Ths code (1) standardizes the continuous features, (2) one hot encodes the categorical features, (3) splits into a train (80%) and test set (20%), (4) based on this data, create another copy where gender is deleted as a predictive feature and the feature we predict is gender (used by SenSR when learning the sensitive directions)
    Input: seed: the seed used to split data into train/test
    """
    # Get the dataset and split into train and test
    dataset_orig = get_adult_data()

    # we will standardize continous features
    continous_features = [
        "age",
        "education-num",
        "capital-gain",
        "capital-loss",
        "hours-per-week",
    ]
    continous_features_indices = [
        dataset_orig.feature_names.index(feat) for feat in continous_features
    ]

    # get a 80%/20% train/test split
    dataset_orig_train, dataset_orig_test = dataset_orig.split(
        [0.8], shuffle=True, seed=seed
    )
    SS = StandardScaler().fit(
        dataset_orig_train.features[:, continous_features_indices]
    )
    dataset_orig_train.features[:, continous_features_indices] = SS.transform(
        dataset_orig_train.features[:, continous_features_indices]
    )
    dataset_orig_test.features[:, continous_features_indices] = SS.transform(
        dataset_orig_test.features[:, continous_features_indices]
    )

    X_train = dataset_orig_train.features
    X_test = dataset_orig_test.features

    y_train = dataset_orig_train.labels.ravel()  # 确保标签是一维的
    y_test = dataset_orig_test.labels.ravel()  # 确保标签是一维的

    X_val = X_train[: len(X_test)]
    y_val = y_train[: len(X_test)]
    X_train = X_train[len(X_test) :]
    y_train = y_train[len(X_test) :]

    # gender id = 39
    A_train = X_train[:, 39]
    A_val = X_val[:, 39]
    A_test = X_test[:, 39]

    X_train, bin_edges = DataQuantize(X_train)
    X_val, _ = DataQuantize(X_val, bin_edges)
    X_test, _ = DataQuantize(X_test, bin_edges)

    return X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test


def preprocess_synthetic_data(seed=0):
    """
    Description: Preprocess the synthetic data for GapReg/DP/EO pipelines
    Returns:
        X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test
    Notes:
        - Features X only contain non-sensitive attributes (x1, x2)
        - Sensitive attribute A is z (binary 0/1)
        - Paths are relative to the adult/ working directory
    """
    # Load raw arrays (xz contains [x1, x2, z])
    xz_train = np.load("../synthetic/xz_train.npy")
    y_train = np.load("../synthetic/y_train.npy")
    z_train = np.load("../synthetic/z_train.npy")

    xz_test = np.load("../synthetic/xz_test.npy")
    y_test = np.load("../synthetic/y_test.npy")
    z_test = np.load("../synthetic/z_test.npy")

    # Split features and sensitive attribute
    X_train = xz_train[:, :2]
    X_test = xz_test[:, :2]

    # Ensure labels are in {0,1}
    y_train = y_train.astype(np.float32).ravel()
    y_test = y_test.astype(np.float32).ravel()
    if (y_train.min() < 0) or (y_train.max() > 1):
        y_train = (y_train > 0).astype(np.float32)
    if (y_test.min() < 0) or (y_test.max() > 1):
        y_test = (y_test > 0).astype(np.float32)

    # Ensure sensitive attribute is in {0,1}
    A_train = z_train.reshape(-1)
    A_test = z_test.reshape(-1)
    if (A_train.min() < 0) or (A_train.max() > 1):
        A_train = (A_train > 0).astype(np.int32)
    else:
        A_train = A_train.astype(np.int32)
    if (A_test.min() < 0) or (A_test.max() > 1):
        A_test = (A_test > 0).astype(np.int32)
    else:
        A_test = A_test.astype(np.int32)

    # Create a reasonable validation set from training (up to 20% of train and not exceeding test size)
    np.random.seed(seed)
    val_size = max(1, int(0.2 * len(X_train)))
    val_size = min(val_size, len(X_test))
    # ensure at least one sample remains for training
    if val_size >= len(X_train):
        val_size = max(1, len(X_train) - 1)
    val_indices = np.random.choice(len(X_train), val_size, replace=False)
    train_indices = np.setdiff1d(np.arange(len(X_train)), val_indices)

    X_val = X_train[val_indices]
    y_val = y_train[val_indices]
    A_val = A_train[val_indices]

    X_train = X_train[train_indices]
    y_train = y_train[train_indices]
    A_train = A_train[train_indices]

    # Standardize non-sensitive features
    SS = StandardScaler().fit(X_train)
    X_train = SS.transform(X_train)
    X_val = SS.transform(X_val)
    X_test = SS.transform(X_test)

    return X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test


def preprocess_synthetic2_data(seed=0, test_k=1):
    """
    读取 synthetic2 数据集（CSV），训练集固定为 multi_train_k4.csv，测试集可在 multi_test_kX.csv 中选择其一。
    支持的测试集编号 X 包括 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4。

    Args:
        seed: 随机种子
        test_k: 选择的测试集编号，支持 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4

    Returns:
        X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test
    """
    valid_test_k_values = [0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4]

    if isinstance(test_k, str):
        try:
            test_k_value = float(test_k)
        except ValueError:
            raise ValueError(
                f"无法解析 test_k='{test_k}'，可选值为 {valid_test_k_values}"
            )
    else:
        test_k_value = float(test_k)

    if not any(np.isclose(test_k_value, v) for v in valid_test_k_values):
        raise ValueError(f"test_k 必须取值于 {valid_test_k_values}，收到: {test_k}")

    # 文件命名规则：整数采用不带小数点的形式，其余保留原有小数格式
    if np.isclose(test_k_value, round(test_k_value)):
        test_k_suffix = str(int(round(test_k_value)))
    else:
        test_k_suffix = f"{test_k_value}".rstrip("0").rstrip(".")

    train_csv = os.path.join("synthetic2", "multi_train_k4.csv")
    # train_csv = os.path.join("synthetic2", "multi_train_k1.csv")
    test_csv = os.path.join("synthetic2", f"multi_test_k{test_k_suffix}.csv")

    # 加载 CSV
    train_df = pd.read_csv(train_csv)
    test_df = pd.read_csv(test_csv)

    # 提取特征与标签、敏感属性
    X_train = train_df[["x1", "x2"]].values
    X_test = test_df[["x1", "x2"]].values

    A_train = train_df["A"].values
    A_test = test_df["A"].values

    y_train = train_df["y"].values.astype(np.float32)
    y_test = test_df["y"].values.astype(np.float32)

    # 规范标签到 {0,1}
    if (y_train.min() < 0) or (y_train.max() > 1):
        y_train = (y_train > 0).astype(np.float32)
    if (y_test.min() < 0) or (y_test.max() > 1):
        y_test = (y_test > 0).astype(np.float32)

    # 规范敏感属性到 {0,1}
    A_train = (
        (A_train > 0).astype(np.int32)
        if (A_train.min() < 0 or A_train.max() > 1)
        else A_train.astype(np.int32)
    )
    A_test = (
        (A_test > 0).astype(np.int32)
        if (A_test.min() < 0 or A_test.max() > 1)
        else A_test.astype(np.int32)
    )

    # 从训练集中划分验证集：按 20% 比例，且不超过测试集大小，保证训练集至少保留 1 条
    np.random.seed(seed)
    val_size = max(1, int(0.2 * len(X_train)))
    val_size = min(val_size, len(X_test))
    if val_size >= len(X_train):
        val_size = max(1, len(X_train) - 1)
    val_indices = np.random.choice(len(X_train), val_size, replace=False)
    train_indices = np.setdiff1d(np.arange(len(X_train)), val_indices)

    X_val = X_train[val_indices]
    y_val = y_train[val_indices]
    A_val = A_train[val_indices]

    X_train = X_train[train_indices]
    y_train = y_train[train_indices]
    A_train = A_train[train_indices]

    # 标准化特征
    SS = StandardScaler().fit(X_train)
    X_train = SS.transform(X_train)
    X_val = SS.transform(X_val)
    X_test = SS.transform(X_test)

    return X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test


def export_synthetic_to_csv_and_plot(seed=0, output_dir="../synthetic/export"):
    """
    调用 preprocess_synthetic_data 读取数据，导出为 CSV，并画一张直观可视化图。

    导出文件：
        - train_full.csv, val_full.csv, test_full.csv，列为 [x1, x2, A, y]
    可视化：
        - synthetic_scatter.png（两列子图：左按 y 着色，右按 A 着色）
    """
    X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
        preprocess_synthetic_data(seed=seed)
    )

    os.makedirs(output_dir, exist_ok=True)

    # 组装并保存 CSV
    def _to_df(X, A, y):
        return pd.DataFrame(
            {"x1": X[:, 0], "x2": X[:, 1], "A": A.astype(int), "y": y.astype(float)}
        )

    train_df = _to_df(X_train, A_train, y_train)
    val_df = _to_df(X_val, A_val, y_val)
    test_df = _to_df(X_test, A_test, y_test)

    train_path = os.path.join(output_dir, "train_full.csv")
    val_path = os.path.join(output_dir, "val_full.csv")
    test_path = os.path.join(output_dir, "test_full.csv")

    train_df.to_csv(train_path, index=False)
    val_df.to_csv(val_path, index=False)
    test_df.to_csv(test_path, index=False)

    # 可视化（一个 Figure，两个子图）：左按 y，右按 A
    fig, axes = plt.subplots(1, 2, figsize=(10, 4))

    # 合并所有分割，便于整体可视化
    all_X = np.vstack([X_train, X_val, X_test])
    all_y = np.concatenate([y_train, y_val, y_test]).astype(int)
    all_A = np.concatenate([A_train, A_val, A_test]).astype(int)

    # 子图1：按 y 着色
    for cls, color, label in [(0, "#1f77b4", "y=0"), (1, "#ff7f0e", "y=1")]:
        mask = all_y == cls
        axes[0].scatter(
            all_X[mask, 0], all_X[mask, 1], s=12, c=color, alpha=0.6, label=label
        )
    axes[0].set_title("Colored by y")
    axes[0].set_xlabel("x1")
    axes[0].set_ylabel("x2")
    axes[0].legend(frameon=False)
    axes[0].grid(alpha=0.2)

    # 子图2：按 A 着色
    for cls, color, label in [(0, "#2ca02c", "A=0"), (1, "#d62728", "A=1")]:
        mask = all_A == cls
        axes[1].scatter(
            all_X[mask, 0], all_X[mask, 1], s=12, c=color, alpha=0.6, label=label
        )
    axes[1].set_title("Colored by A")
    axes[1].set_xlabel("x1")
    axes[1].set_ylabel("x2")
    axes[1].legend(frameon=False)
    axes[1].grid(alpha=0.2)

    plt.tight_layout()
    fig_path = os.path.join(output_dir, "synthetic_scatter.png")
    plt.savefig(fig_path, dpi=150)
    plt.close(fig)

    # 训练集散点图（按 y 与按 A）
    fig_tr, axes_tr = plt.subplots(1, 2, figsize=(10, 4))
    y_tr = y_train.astype(int)
    A_tr = A_train.astype(int)
    for cls, color, label in [(0, "#1f77b4", "y=0"), (1, "#ff7f0e", "y=1")]:
        mask = y_tr == cls
        axes_tr[0].scatter(
            X_train[mask, 0], X_train[mask, 1], s=12, c=color, alpha=0.6, label=label
        )
    axes_tr[0].set_title("Train colored by y")
    axes_tr[0].set_xlabel("x1")
    axes_tr[0].set_ylabel("x2")
    axes_tr[0].legend(frameon=False)
    axes_tr[0].grid(alpha=0.2)

    for cls, color, label in [(0, "#2ca02c", "A=0"), (1, "#d62728", "A=1")]:
        mask = A_tr == cls
        axes_tr[1].scatter(
            X_train[mask, 0], X_train[mask, 1], s=12, c=color, alpha=0.6, label=label
        )
    axes_tr[1].set_title("Train colored by A")
    axes_tr[1].set_xlabel("x1")
    axes_tr[1].set_ylabel("x2")
    axes_tr[1].legend(frameon=False)
    axes_tr[1].grid(alpha=0.2)

    plt.tight_layout()
    fig_tr_path = os.path.join(output_dir, "synthetic_scatter_train.png")
    plt.savefig(fig_tr_path, dpi=150)
    plt.close(fig_tr)

    # 测试集散点图（按 y 与按 A）
    fig_te, axes_te = plt.subplots(1, 2, figsize=(10, 4))
    y_te = y_test.astype(int)
    A_te = A_test.astype(int)
    for cls, color, label in [(0, "#1f77b4", "y=0"), (1, "#ff7f0e", "y=1")]:
        mask = y_te == cls
        axes_te[0].scatter(
            X_test[mask, 0], X_test[mask, 1], s=12, c=color, alpha=0.6, label=label
        )
    axes_te[0].set_title("Test colored by y")
    axes_te[0].set_xlabel("x1")
    axes_te[0].set_ylabel("x2")
    axes_te[0].legend(frameon=False)
    axes_te[0].grid(alpha=0.2)

    for cls, color, label in [(0, "#2ca02c", "A=0"), (1, "#d62728", "A=1")]:
        mask = A_te == cls
        axes_te[1].scatter(
            X_test[mask, 0], X_test[mask, 1], s=12, c=color, alpha=0.6, label=label
        )
    axes_te[1].set_title("Test colored by A")
    axes_te[1].set_xlabel("x1")
    axes_te[1].set_ylabel("x2")
    axes_te[1].legend(frameon=False)
    axes_te[1].grid(alpha=0.2)

    plt.tight_layout()
    fig_te_path = os.path.join(output_dir, "synthetic_scatter_test.png")
    plt.savefig(fig_te_path, dpi=150)
    plt.close(fig_te)

    print(f"Saved CSVs to:\n - {train_path}\n - {val_path}\n - {test_path}")
    print(f"Saved figure to: {fig_path}")
    print(f"Saved train figure to: {fig_tr_path}")
    print(f"Saved test figure to: {fig_te_path}")


def plot_synthetic_y_A_distribution(
    seed=0, output_dir="../synthetic/export", normalize=True
):
    """
    绘制训练集与测试集的 y 与 A 分布对比图（忽略 x1/x2）。

    参数：
        - normalize: True 输出比例，False 输出计数
    输出：
        - synthetic_distribution.png（左：y分布，右：A分布；训练/测试并列柱状图）
    """
    os.makedirs(output_dir, exist_ok=True)

    X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
        preprocess_synthetic_data(seed=seed)
    )

    # 只关心 train/test 的 y 与 A
    def ensure_len2_bincount(arr):
        bc = np.bincount(arr.astype(int), minlength=2)
        if len(bc) < 2:
            bc = np.pad(bc, (0, 2 - len(bc)), constant_values=0)
        return bc[:2]

    y_tr_cnt = ensure_len2_bincount(y_train)
    y_te_cnt = ensure_len2_bincount(y_test)
    A_tr_cnt = ensure_len2_bincount(A_train)
    A_te_cnt = ensure_len2_bincount(A_test)

    if normalize:
        y_tr = y_tr_cnt / max(1, y_tr_cnt.sum())
        y_te = y_te_cnt / max(1, y_te_cnt.sum())
        A_tr = A_tr_cnt / max(1, A_tr_cnt.sum())
        A_te = A_te_cnt / max(1, A_te_cnt.sum())
        y_label = "Proportion"
    else:
        y_tr, y_te = y_tr_cnt, y_te_cnt
        A_tr, A_te = A_tr_cnt, A_te_cnt
        y_label = "Count"

    x = np.arange(2)
    width = 0.35

    fig, axes = plt.subplots(1, 2, figsize=(9, 4))

    # y 分布对比
    axes[0].bar(x - width / 2, y_tr, width, label="Train")
    axes[0].bar(x + width / 2, y_te, width, label="Test")
    axes[0].set_xticks(x)
    axes[0].set_xticklabels(["0", "1"])
    axes[0].set_title("y distribution (Train vs Test)")
    axes[0].set_xlabel("y")
    axes[0].set_ylabel(y_label)
    axes[0].legend(frameon=False)
    axes[0].grid(alpha=0.2, axis="y")

    # A 分布对比
    axes[1].bar(x - width / 2, A_tr, width, label="Train")
    axes[1].bar(x + width / 2, A_te, width, label="Test")
    axes[1].set_xticks(x)
    axes[1].set_xticklabels(["0", "1"])
    axes[1].set_title("A distribution (Train vs Test)")
    axes[1].set_xlabel("A")
    axes[1].set_ylabel(y_label)
    axes[1].legend(frameon=False)
    axes[1].grid(alpha=0.2, axis="y")

    plt.tight_layout()
    out_path = os.path.join(output_dir, "synthetic_distribution.png")
    plt.savefig(out_path, dpi=150)
    plt.close(fig)

    print(f"Saved distribution figure to: {out_path}")


def plot_synthetic_y_A_distribution_scatter(
    seed=0, output_dir="../synthetic/export", normalize=True
):
    """
    用散点图对比训练集与测试集的 y 和 A 分布（忽略 x1/x2）。
    - x 轴为类别 {0,1}
    - y 轴为数据集 {Train, Test}
    - 点大小按比例/计数缩放，并在点上标数值
    输出：synthetic_distribution_scatter.png
    """
    os.makedirs(output_dir, exist_ok=True)

    X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
        preprocess_synthetic_data(seed=seed)
    )

    def ensure_len2_bincount(arr):
        bc = np.bincount(arr.astype(int), minlength=2)
        if len(bc) < 2:
            bc = np.pad(bc, (0, 2 - len(bc)), constant_values=0)
        return bc[:2]

    # 统计计数
    y_tr_cnt = ensure_len2_bincount(y_train)
    y_te_cnt = ensure_len2_bincount(y_test)
    A_tr_cnt = ensure_len2_bincount(A_train)
    A_te_cnt = ensure_len2_bincount(A_test)

    if normalize:
        y_tr = y_tr_cnt / max(1, y_tr_cnt.sum())
        y_te = y_te_cnt / max(1, y_te_cnt.sum())
        A_tr = A_tr_cnt / max(1, A_tr_cnt.sum())
        A_te = A_te_cnt / max(1, A_te_cnt.sum())
        y_label = "Proportion"
    else:
        y_tr, y_te = y_tr_cnt, y_te_cnt
        A_tr, A_te = A_tr_cnt, A_te_cnt
        y_label = "Count"

    # 计算气泡大小缩放
    vals = np.concatenate([y_tr, y_te, A_tr, A_te]).astype(float)
    vmax = max(1e-8, vals.max())

    # 基础大小，保证最小点可见
    def size(v):
        return 1500.0 * (float(v) / vmax + 0.05)

    # 布局
    fig, axes = plt.subplots(1, 2, figsize=(10, 4))
    xticks = np.array([0, 1])
    yticks = np.array([0, 1])  # 0: Train, 1: Test

    # 左：y 分布
    for c in [0, 1]:
        axes[0].scatter(
            [c],
            [0],
            s=size(y_tr[c]),
            c="#1f77b4",
            alpha=0.7,
            edgecolor="white",
            linewidth=0.5,
        )
        axes[0].scatter(
            [c],
            [1],
            s=size(y_te[c]),
            c="#ff7f0e",
            alpha=0.7,
            edgecolor="white",
            linewidth=0.5,
        )
        # 标注数值
        axes[0].text(
            c,
            0,
            f"{y_tr[c]:.3f}" if normalize else f"{int(y_tr[c])}",
            ha="center",
            va="center",
            color="white",
            fontsize=9,
        )
        axes[0].text(
            c,
            1,
            f"{y_te[c]:.3f}" if normalize else f"{int(y_te[c])}",
            ha="center",
            va="center",
            color="white",
            fontsize=9,
        )
    axes[0].set_xticks(xticks)
    axes[0].set_xticklabels(["0", "1"])
    axes[0].set_yticks(yticks)
    axes[0].set_yticklabels(["Train", "Test"])
    axes[0].set_xlim(-0.5, 1.5)
    axes[0].set_ylim(-0.5, 1.5)
    axes[0].set_title("y distribution (scatter)")
    axes[0].set_xlabel("y")
    axes[0].set_ylabel("Dataset")
    axes[0].grid(alpha=0.2)

    # 右：A 分布
    for c in [0, 1]:
        axes[1].scatter(
            [c],
            [0],
            s=size(A_tr[c]),
            c="#2ca02c",
            alpha=0.7,
            edgecolor="white",
            linewidth=0.5,
        )
        axes[1].scatter(
            [c],
            [1],
            s=size(A_te[c]),
            c="#d62728",
            alpha=0.7,
            edgecolor="white",
            linewidth=0.5,
        )
        axes[1].text(
            c,
            0,
            f"{A_tr[c]:.3f}" if normalize else f"{int(A_tr[c])}",
            ha="center",
            va="center",
            color="white",
            fontsize=9,
        )
        axes[1].text(
            c,
            1,
            f"{A_te[c]:.3f}" if normalize else f"{int(A_te[c])}",
            ha="center",
            va="center",
            color="white",
            fontsize=9,
        )
    axes[1].set_xticks(xticks)
    axes[1].set_xticklabels(["0", "1"])
    axes[1].set_yticks(yticks)
    axes[1].set_yticklabels(["Train", "Test"])
    axes[1].set_xlim(-0.5, 1.5)
    axes[1].set_ylim(-0.5, 1.5)
    axes[1].set_title("A distribution (scatter)")
    axes[1].set_xlabel("A")
    axes[1].set_ylabel("Dataset")
    axes[1].grid(alpha=0.2)

    plt.tight_layout()
    out_path = os.path.join(output_dir, "synthetic_distribution_scatter.png")
    plt.savefig(out_path, dpi=150)
    plt.close(fig)

    print(f"Saved distribution scatter figure to: {out_path}")


def summarize_synthetic_yA_counts(
    seed=0, output_dir="../synthetic/export", save_csv=True
):
    """
    统计训练集和测试集在以下四种组合的样本数：
        - y=1, A=1
        - y=0, A=0
        - y=0, A=1
        - y=1, A=0
    输出：打印表格；如 save_csv=True，保存 CSV 'synthetic_yA_counts.csv'
    """
    os.makedirs(output_dir, exist_ok=True)

    X_train, X_val, X_test, y_train, y_val, y_test, A_train, A_val, A_test = (
        preprocess_synthetic_data(seed=seed)
    )

    def combo_counts(y, A):
        y = y.astype(int)
        A = A.astype(int)
        return {
            "y=1,A=1": int(np.sum((y == 1) & (A == 1))),
            "y=0,A=0": int(np.sum((y == 0) & (A == 0))),
            "y=0,A=1": int(np.sum((y == 0) & (A == 1))),
            "y=1,A=0": int(np.sum((y == 1) & (A == 0))),
        }

    train_counts = combo_counts(y_train, A_train)
    test_counts = combo_counts(y_test, A_test)

    # 构建 DataFrame（数量 + 比例 + 总数）
    train_total = max(1, int(len(y_train)))
    test_total = max(1, int(len(y_test)))

    def add_props(d, total):
        return {f"{k}_prop": (v / total) for k, v in d.items()}

    train_props = add_props(train_counts, train_total)
    test_props = add_props(test_counts, test_total)

    df = pd.DataFrame(
        [
            {"set": "train", **train_counts, **train_props, "total": train_total},
            {"set": "test", **test_counts, **test_props, "total": test_total},
        ]
    )

    # 打印
    print(df.to_string(index=False))

    # 保存 CSV
    if save_csv:
        csv_path = os.path.join(output_dir, "synthetic_yA_counts.csv")
        df.to_csv(csv_path, index=False)
        print(f"Saved counts CSV to: {csv_path}")

    return df


if __name__ == "__main__":
    export_synthetic_to_csv_and_plot(seed=0, output_dir="../synthetic/export")
