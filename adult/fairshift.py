import itertools

import numpy as np
import torch


def correlation_reweighting(xz_data, y_data, z_data, w, w_new):
    z_item = list(set(z_data.tolist()))
    y_item = list(set(y_data.tolist()))
    yz_tuple = list(itertools.product(y_item, z_item))

    z_mask = {}
    y_mask = {}
    yz_mask = {}

    for tmp_z in z_item:
        z_mask[tmp_z] = z_data == tmp_z

    for tmp_y in y_item:
        y_mask[tmp_y] = y_data == tmp_y

    for tmp_yz in yz_tuple:
        yz_mask[tmp_yz] = (y_data == tmp_yz[0]) & (z_data == tmp_yz[1])

    ex_weight = []
    for i in range(len(y_data)):
        if yz_mask[-1, 0][i] == 1:
            ex_weight.append(w_new[3] / w[3])
        elif yz_mask[1, 0][i] == 1:
            ex_weight.append(w_new[1] / w[1])
        elif yz_mask[-1, 1][i] == 1:
            ex_weight.append(w_new[2] / w[2])
        else:
            ex_weight.append(w_new[0] / w[0])

    return torch.FloatTensor(ex_weight)


def datasampling(xz_data, y_data, z_data, ex_weights, seed=0):
    np.random.seed(seed)

    z_item = list(set(z_data.tolist()))
    y_item = list(set(y_data.tolist()))
    yz_tuple = list(itertools.product(y_item, z_item))

    z_mask = {}
    y_mask = {}
    yz_mask = {}

    for tmp_z in z_item:
        z_mask[tmp_z] = z_data == tmp_z

    for tmp_y in y_item:
        y_mask[tmp_y] = y_data == tmp_y

    for tmp_yz in yz_tuple:
        yz_mask[tmp_yz] = (y_data == tmp_yz[0]) & (z_data == tmp_yz[1])

    yz_index = {}
    for tmp_yz in yz_tuple:
        yz_index[tmp_yz] = (yz_mask[tmp_yz] == 1).nonzero().squeeze()

    selected_index = []
    max_weight = max(ex_weights)
    for tmp_yz in yz_tuple:
        class_indices = yz_index[tmp_yz].cpu()
        if class_indices.numel() == 0:
            continue
        chosen = np.random.choice(
            class_indices,
            int(len(class_indices) * ex_weights[class_indices[0]] / max_weight),
            replace=False,
        )
        selected_index.extend(chosen)

    return selected_index


# Ordering used in the original Fair-Shift implementation
_CLASS_ORDER = [
    (1.0, 1.0),  # y = 1, z = 1
    (1.0, 0.0),  # y = 1, z = 0
    (-1.0, 1.0),  # y = -1, z = 1
    (-1.0, 0.0),  # y = -1, z = 0
]

_BINARY_ORDER = [
    (0, 0),
    (0, 1),
    (1, 0),
    (1, 1),
]


def _to_pm_one(y: np.ndarray) -> np.ndarray:
    """Convert {0,1} labels to {-1, 1} as expected by Fair-Shift utils."""

    y_arr = np.asarray(y).astype(float)
    if not np.all(np.isin(y_arr, [0.0, 1.0])):
        raise ValueError("y must be binary in {0,1} to convert to {-1,1}")
    return np.where(y_arr > 0.0, 1.0, -1.0)


def _compute_ratio(y_pm: np.ndarray, a: np.ndarray) -> np.ndarray:
    """Compute class ratios following the Fair-Shift class order."""

    total = len(y_pm)
    if total == 0:
        raise ValueError("Input arrays must be non-empty")

    ratios = []
    for y_val, a_val in _CLASS_ORDER:
        count = np.sum((y_pm == y_val) & (a == a_val))
        ratios.append(count / total)
    return np.asarray(ratios, dtype=np.float64)


def _ensure_no_zero_ratio(ratios: np.ndarray, name: str):
    if np.any(ratios == 0):
        missing = [(y, z) for (y, z), r in zip(_CLASS_ORDER, ratios) if r == 0]
        raise ValueError(
            f"{name} has zero probability for classes {missing}; "
            "original Fair-Shift procedure requires non-empty classes."
        )


def _compute_binary_joint_ratios(
    y: np.ndarray, a: np.ndarray
) -> dict[tuple[int, int], float]:
    total = len(y)
    if total == 0:
        raise ValueError("Input arrays must be non-empty")

    ratios: dict[tuple[int, int], float] = {}
    for combo in _BINARY_ORDER:
        combo_count = np.sum((y == combo[0]) & (a == combo[1]))
        ratios[combo] = combo_count / total
    return ratios


def _log_binary_ratios(title: str, ratios: dict[tuple[int, int], float]):
    print(title)
    for y_val, a_val in _BINARY_ORDER:
        ratio = ratios.get((y_val, a_val), 0.0)
        print(f"    (y={y_val}, A={a_val}) 比例: {ratio:.4f}")


def align_train_with_target(
    X_train: np.ndarray,
    y_train: np.ndarray,
    a_train: np.ndarray,
    y_target: np.ndarray,
    a_target: np.ndarray,
    random_state: int | None = None,
):
    """Apply Fair-Shift preprocessing using the original utils pipeline.

    Steps follow Fair-Shift's `correlation_reweighting` + `datasampling`:
        1. Compute original joint ratios w on training data.
        2. Compute target joint ratios w_new.
        3. Use `correlation_reweighting` to obtain per-example weights.
        4. Call `datasampling` to sample indices accordingly.

    Returns resampled X, y, a numpy arrays.
    """

    rng_state = np.random.get_state()
    if random_state is not None:
        np.random.seed(random_state)

    try:
        y_train_arr = np.asarray(y_train).astype(int)
        a_train_arr_binary = np.asarray(a_train).astype(int)
        before_ratios = _compute_binary_joint_ratios(y_train_arr, a_train_arr_binary)
        _log_binary_ratios("[FairShift] 预处理前训练集 (y, A) 分布:", before_ratios)

        y_train_pm = _to_pm_one(y_train)
        y_target_pm = _to_pm_one(y_target)
        a_train_arr = np.asarray(a_train).astype(float)
        a_target_arr = np.asarray(a_target).astype(float)

        w = _compute_ratio(y_train_pm, a_train_arr)
        w_new = _compute_ratio(y_target_pm, a_target_arr)

        _ensure_no_zero_ratio(w, "Training distribution")
        _ensure_no_zero_ratio(w_new, "Target distribution")

        x_tensor = torch.from_numpy(np.asarray(X_train)).float()
        y_tensor = torch.from_numpy(y_train_pm).float()
        z_tensor = torch.from_numpy(a_train_arr).float()

        weights = correlation_reweighting(
            x_tensor,
            y_tensor,
            z_tensor,
            w=w.tolist(),
            w_new=w_new.tolist(),
        )

        indices = datasampling(
            x_tensor,
            y_tensor,
            z_tensor,
            weights,
            seed=random_state if random_state is not None else 0,
        )

        indices = torch.tensor(indices, dtype=torch.long)

        X_resampled = x_tensor[indices].numpy()
        y_resampled = (y_tensor[indices].numpy() > 0).astype(int)
        a_resampled = z_tensor[indices].numpy().astype(int)

        after_ratios = _compute_binary_joint_ratios(y_resampled, a_resampled)
        _log_binary_ratios("[FairShift] 预处理后训练集 (y, A) 分布:", after_ratios)

        return X_resampled, y_resampled, a_resampled
    finally:
        np.random.set_state(rng_state)
