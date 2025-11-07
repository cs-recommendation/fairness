import numpy as np  # 数值计算库
import torch  # PyTorch 主库
import torch.nn as nn  # 神经网络模块（用于导出预测器）
import torch.nn.functional as F  # 神经网络函数
from scipy.optimize import linprog  # 线性规划求解器（用于样本删除/重加权）
from typing import Optional  # 可选类型注解
from model import Net  # 引入神经网络模型


class ReweightTrainer:  # 重加权训练器：基于影响函数 + 线性规划
    def __init__(
        self,
        metric: str = "dp",
        alpha: float = 0.1,
        beta: float = 1.0,
        gamma: float = 1.0,
        l2_reg: float = 1.0,
        pretrain_epochs: int = 30,
        pretrain_lr: float = 1e-3,
        pretrain_batch_size: int = 256,
        pretrain_iterations: Optional[int] = None,
        finetune_epochs: int = 100,
        finetune_lr: float = 5e-3,
        finetune_batch_size: int = 256,
        finetune_iterations: Optional[int] = None,
        experiments: int = 1,
        random_state: Optional[int] = None,
    ):
        if metric not in {"dp", "eo"}:
            raise ValueError(f"Unsupported metric: {metric}")
        self.metric = metric  # 公平性度量选择
        self.alpha = alpha  # 线性规划约束中的样本删除上限系数
        self.beta = beta  # 公平性改进预算的缩放系数
        self.gamma = gamma  # 效用保持预算的缩放系数
        self.random_state = random_state  # 随机种子
        if random_state is not None:
            np.random.seed(random_state)
        self.l2_reg = float(l2_reg)  # 仅用于最后一层的 L2 正则
        self.weights_: Optional[np.ndarray] = None  # 最终得到的样本权重（1-删除概率）
        self.nn: Optional[Net] = None  # 神经网络
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        # 训练超参
        self.pretrain_epochs = int(pretrain_epochs)
        self.pretrain_lr = float(pretrain_lr)
        self.pretrain_batch_size = int(pretrain_batch_size)
        self.pretrain_iterations = (
            None if pretrain_iterations is None else int(pretrain_iterations)
        )
        self.finetune_epochs = int(finetune_epochs)
        self.finetune_lr = float(finetune_lr)
        self.finetune_batch_size = int(finetune_batch_size)
        self.finetune_iterations = (
            None if finetune_iterations is None else int(finetune_iterations)
        )
        self.experiments = int(experiments)

    def fit(
        self,
        X_train: np.ndarray,
        y_train: np.ndarray,
        A_train: np.ndarray,
        X_val: np.ndarray,
        y_val: np.ndarray,
        A_val: np.ndarray,
    ) -> None:
        # 1) 预训练：在函数内部使用 experiments/epochs/iterations/batch_size，选择验证 gap 最小的 epoch/experiment
        self._pretrain_network(
            X_train,
            y_train,
            X_val,
            y_val,
            A_val,
            epochs=self.pretrain_epochs,
            lr=self.pretrain_lr,
            batch_size=self.pretrain_batch_size,
            iterations=self.pretrain_iterations,
            experiments=self.experiments,
        )

        # 2) 基于最佳预训练参数计算一次影响与样本权重
        Z_train = self._extract_features(X_train)
        Z_val = self._extract_features(X_val)
        w, b = self._get_last_layer_params()
        prob_val = self._predict_proba_from_features(Z_val, w, b)
        prob_train = self._predict_proba_from_features(Z_train, w, b)
        if self.metric == "eo":
            fair_loss_val = loss_ferm(X_val, y_val, A_val, prob_val)
        else:
            fair_loss_val = loss_dp(X_val, A_val, prob_val)
        util_grad_total, _ = self._grad_on_features(Z_val, y_val, prob_val)
        if self.metric == "eo":
            fair_grad_total = grad_ferm(
                self._grad_pred_on_features, Z_val, y_val, A_val
            )
        else:
            fair_grad_total = grad_dp(self._grad_pred_on_features, Z_val, A_val)
        hessian = self._hess_on_features(Z_train, prob_train)
        util_grad_hvp = self._solve_linear(hessian, util_grad_total)
        fair_grad_hvp = self._solve_linear(hessian, fair_grad_total)
        _, train_indiv_grad = self._grad_on_features(Z_train, y_train, prob_train)
        util_influence = train_indiv_grad @ util_grad_hvp
        fair_influence = train_indiv_grad @ fair_grad_hvp
        removal_prob = self._solve_lp(fair_influence, util_influence, fair_loss_val)
        self.weights_ = np.clip(1.0 - removal_prob, 0.0, 1.0).astype(np.float64)

        # 3) 重训练：仅最后一层，仅考虑 epochs，选择验证 gap 最小的 epoch 作为最终 fc3 参数
        self._retrain_last_layer(
            X_train,
            y_train,
            self.weights_,
            epochs=self.finetune_epochs,
            X_val=X_val,
            y_val=y_val,
            A_val=A_val,
        )

    def build_predictor(self) -> nn.Module:  # 返回神经网络（评估模式）
        assert self.nn is not None
        self.nn.to(self.device)
        self.nn.eval()
        for p in self.nn.parameters():
            p.requires_grad_(False)
        return self.nn

    def _solve_lp(
        self, fair_infl: np.ndarray, util_infl: np.ndarray, fair_loss: float
    ) -> np.ndarray:  # 线性规划：求解最优“删除概率” r ∈ [0,1]^n
        num_sample = fair_infl.shape[0]  # 样本数
        bounds = [(0.0, 1.0)] * num_sample  # 变量上下界：每个 r_i ∈ [0,1]
        all_one = np.ones(num_sample)  # 全 1 向量

        # 负影响之和：代表可改善公平/效用的最大潜力（近似上界）
        max_fair = fair_infl[fair_infl < 0.0].sum() if np.any(fair_infl < 0.0) else 0.0
        max_util = util_infl[util_infl < 0.0].sum() if np.any(util_infl < 0.0) else 0.0

        if fair_loss >= -max_fair:  # 当当前公平损失已不比“最好能达到的”更差时
            c = fair_infl.astype(np.float64)  # 目标：优先去除对公平不利的样本
            A_ub = np.vstack(
                [
                    util_infl.reshape(1, -1),  # 约束：效用不下降（近似）
                    all_one.reshape(1, -1),  # 约束：删除样本数量上限
                ]
            )
            b_ub = np.array([0.0, self.alpha * num_sample], dtype=np.float64)
        else:  # 公平损失较大：允许更多删除以换取更大公平改善
            c = all_one.astype(np.float64)  # 目标：在预算内最小化删除量
            A_ub = np.vstack(
                [
                    fair_infl.reshape(1, -1),  # 约束：公平改进达标
                    util_infl.reshape(1, -1),  # 约束：效用下降受限
                ]
            )
            b_ub = np.array(
                [
                    self.beta * (-fair_loss),  # 所需公平改进的预算
                    self.gamma * max_util,  # 允许的效用下降预算
                ],
                dtype=np.float64,
            )

        res = linprog(
            c=c, A_ub=A_ub, b_ub=b_ub, bounds=bounds, method="highs"
        )  # 调用 HiGHS 求解
        if res.success and res.x is not None:
            return res.x.astype(np.float64)  # 删除概率 r
        return np.zeros(num_sample, dtype=np.float64)  # 失败则不删除

    # ==================== 神经网络适配：仅最后一层的影响与再训练 ====================
    def _pretrain_network(
        self,
        X: np.ndarray,
        y: np.ndarray,
        X_val: np.ndarray,
        y_val: np.ndarray,
        A_val: np.ndarray,
        epochs: int = 30,
        lr: float = 1e-3,
        batch_size: int = 256,
        iterations: Optional[int] = None,
        experiments: int = 1,
    ) -> None:
        best_state: Optional[dict] = None
        best_gap: float = np.inf

        for exp in range(int(experiments)):
            # 重新初始化网络
            self.nn = Net(input_size=X.shape[1]).to(self.device)
            optimizer = torch.optim.Adam(self.nn.parameters(), lr=lr)
            X_tensor = torch.from_numpy(X.astype(np.float32))
            y_tensor = torch.from_numpy(y.astype(np.float32)).view(-1, 1)
            dataset = torch.utils.data.TensorDataset(X_tensor, y_tensor)
            loader = torch.utils.data.DataLoader(
                dataset, batch_size=batch_size, shuffle=True, drop_last=False
            )

            # 每个 experiment 内挑选最佳 epoch（按验证 gap，跳过第1个 epoch）
            exp_best_gap = np.inf
            exp_best_state = None

            for ep in range(int(epochs)):
                self.nn.train()
                step = 0
                for xb, yb in loader:
                    xb = xb.to(self.device)
                    yb = yb.to(self.device)
                    pred = self.nn(xb)
                    loss = F.binary_cross_entropy(pred, yb)
                    optimizer.zero_grad()
                    loss.backward()
                    optimizer.step()
                    step += 1
                    if iterations is not None and step >= iterations:
                        break

                # 验证
                self.nn.eval()
                if ep > 0:
                    from utils import evaluate_dp as _eval_dp, evaluate_eo as _eval_eo

                    if self.metric == "dp":
                        _, gap_val = _eval_dp(self.nn, X_val, y_val, A_val)
                    else:
                        _, gap_val, _, _ = _eval_eo(self.nn, X_val, y_val, A_val)
                    if gap_val < exp_best_gap:
                        exp_best_gap = gap_val
                        exp_best_state = {
                            k: v.detach().cpu().clone()
                            for k, v in self.nn.state_dict().items()
                        }

            # 与全局最优 experiment 对比
            if exp_best_state is not None and exp_best_gap < best_gap:
                best_gap = exp_best_gap
                best_state = exp_best_state

        # 加载全局预训练最佳参数并冻结前两层
        if best_state is not None:
            self.nn.load_state_dict(best_state)
        assert isinstance(self.nn, Net)
        for p in self.nn.fc1.parameters():
            p.requires_grad_(False)
        for p in self.nn.fc2.parameters():
            p.requires_grad_(False)

    def _extract_features(self, X: np.ndarray) -> np.ndarray:
        assert self.nn is not None
        self.nn.eval()
        with torch.no_grad():
            xt = torch.from_numpy(X.astype(np.float32)).to(self.device)
            z = self.nn.fc1(xt)
            z = F.relu(z)
            z = self.nn.fc2(z)
            z = F.relu(z)
        return z.detach().cpu().numpy()

    def _get_last_layer_params(self):
        assert self.nn is not None
        with torch.no_grad():
            w = self.nn.fc3.weight.detach().cpu().numpy().reshape(-1)
            b = float(self.nn.fc3.bias.detach().cpu().numpy().reshape(()))
        return w, b

    def _set_last_layer_params(self, w: np.ndarray, b: float) -> None:
        assert self.nn is not None
        with torch.no_grad():
            self.nn.fc3.weight.copy_(torch.from_numpy(w.reshape(1, -1)).to(self.device))
            self.nn.fc3.bias.copy_(
                torch.tensor([b], dtype=torch.float32, device=self.device)
            )

    @staticmethod
    def _sigmoid(x: np.ndarray) -> np.ndarray:
        return 1.0 / (1.0 + np.exp(-x))

    def _predict_proba_from_features(
        self, Z: np.ndarray, w: np.ndarray, b: float
    ) -> np.ndarray:
        logits = Z @ w + b
        return self._sigmoid(logits)

    def _augment_features(self, Z: np.ndarray) -> np.ndarray:
        ones = np.ones((Z.shape[0], 1), dtype=Z.dtype)
        return np.concatenate([Z, ones], axis=1)

    def _pack_params(self, w: np.ndarray, b: float) -> np.ndarray:
        return np.concatenate([w, np.array([b], dtype=np.float64)], axis=0)

    def _unpack_params(self, theta: np.ndarray):
        return theta[:-1], float(theta[-1])

    def _grad_on_features(
        self, Z: np.ndarray, y: np.ndarray, prob: np.ndarray
    ):  # 逻辑回归最后一层的梯度（含 L2，仅权重）
        phi = self._augment_features(Z)
        diff = prob - y
        indiv_grad = phi * diff[:, None]
        total_grad = indiv_grad.sum(axis=0)
        w, b = self._get_last_layer_params()
        reg = np.concatenate([self.l2_reg * w, np.array([0.0])], axis=0)
        total_grad = total_grad + reg
        return total_grad, indiv_grad

    def _grad_pred_on_features(
        self, Z: np.ndarray
    ):  # 对概率 p 的梯度（用于 dp/eo 的梯度项）
        w, b = self._get_last_layer_params()
        prob = self._predict_proba_from_features(Z, w, b)
        phi = self._augment_features(Z)
        factor = prob * (1.0 - prob)
        indiv_grad = phi * factor[:, None]
        total_grad = indiv_grad.sum(axis=0)
        return total_grad, indiv_grad

    def _hess_on_features(
        self, Z: np.ndarray, prob: np.ndarray
    ):  # 最后一层 Hessian（含 L2，仅权重）
        phi = self._augment_features(Z)
        factor = prob * (1.0 - prob)
        H = phi.T @ (factor[:, None] * phi)
        d_plus_bias = phi.shape[1]
        H_reg = np.zeros((d_plus_bias, d_plus_bias), dtype=np.float64)
        H_reg[: d_plus_bias - 1, : d_plus_bias - 1] = self.l2_reg * np.eye(
            d_plus_bias - 1
        )
        return H + H_reg

    def _retrain_last_layer(
        self,
        X: np.ndarray,
        y: np.ndarray,
        sample_weight: np.ndarray,
        epochs: int = 100,
        lr: float = 5e-3,
        X_val: Optional[np.ndarray] = None,
        y_val: Optional[np.ndarray] = None,
        A_val: Optional[np.ndarray] = None,
    ) -> None:
        assert self.nn is not None
        # 冻结前两层，仅更新最后一层
        self.nn.train()
        for p in self.nn.fc1.parameters():
            p.requires_grad_(False)
        for p in self.nn.fc2.parameters():
            p.requires_grad_(False)
        for p in self.nn.fc3.parameters():
            p.requires_grad_(True)

        optimizer = torch.optim.Adam(self.nn.fc3.parameters(), lr=lr)
        X_tensor = torch.from_numpy(X.astype(np.float32)).to(self.device)
        y_tensor = torch.from_numpy(y.astype(np.float32)).view(-1, 1).to(self.device)
        w_tensor = (
            torch.from_numpy(sample_weight.astype(np.float32))
            .view(-1, 1)
            .to(self.device)
        )

        best_gap = np.inf
        best_fc3_w = None
        best_fc3_b = None

        for ep in range(int(epochs)):
            # 全量一次前向（不使用 batch_size/iterations 超参）
            self.nn.train()
            pred = self.nn(X_tensor)
            eps = 1e-12
            loss_vec = -(
                y_tensor * torch.log(pred + eps)
                + (1.0 - y_tensor) * torch.log(1.0 - pred + eps)
            )
            loss = (w_tensor * loss_vec).sum() / (w_tensor.sum() + eps)
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

            # 验证并选择最佳 epoch（跳过第1个 epoch）
            self.nn.eval()
            if X_val is not None and y_val is not None and A_val is not None and ep > 0:
                from utils import evaluate_dp as _eval_dp, evaluate_eo as _eval_eo

                if self.metric == "dp":
                    _, gap_val = _eval_dp(self.nn, X_val, y_val, A_val)
                else:
                    _, gap_val, _, _ = _eval_eo(self.nn, X_val, y_val, A_val)
                if gap_val < best_gap:
                    best_gap = gap_val
                    # 仅保存最后一层参数
                    best_fc3_w = self.nn.fc3.weight.detach().cpu().clone()
                    best_fc3_b = self.nn.fc3.bias.detach().cpu().clone()

        # 恢复最佳 epoch 的最后一层参数
        if best_fc3_w is not None and best_fc3_b is not None:
            with torch.no_grad():
                self.nn.fc3.weight.copy_(best_fc3_w.to(self.device))
                self.nn.fc3.bias.copy_(best_fc3_b.to(self.device))

    def _grad(
        self, X: np.ndarray, y: np.ndarray, prob: np.ndarray
    ):  # 兼容旧接口：映射到特征版
        return self._grad_on_features(X, y, prob)

    def _grad_wrapper(self, X: np.ndarray, y: np.ndarray):  # 包装：在特征空间计算梯度
        w, b = self._get_last_layer_params()
        prob = self._predict_proba_from_features(X, w, b)
        total_grad, indiv = self._grad_on_features(X, y, prob)
        return total_grad, indiv

    def _grad_pred_wrapper(
        self, X: np.ndarray
    ):  # 仅依赖预测概率的梯度（用于 dp），映射到特征版
        return self._grad_pred_on_features(X)

    def _hess(self, X: np.ndarray, prob: np.ndarray):  # Hessian 兼容：映射到特征版
        return self._hess_on_features(X, prob)

    @staticmethod
    def _solve_linear(
        hessian: np.ndarray, vector: np.ndarray
    ) -> np.ndarray:  # 解线性方程 Hx=g
        try:
            return np.linalg.solve(hessian, vector)  # 首选精确求解
        except np.linalg.LinAlgError:
            hessian_jitter = (
                hessian + np.eye(hessian.shape[0]) * 1e-6
            )  # Jitter 提升数值稳定性
            try:
                return np.linalg.solve(hessian_jitter, vector)
            except np.linalg.LinAlgError:
                return hessian_jitter.T @ vector  # 退化到一次共轭近似


def loss_dp(
    X: np.ndarray, s: np.ndarray, pred: np.ndarray
) -> float:  # 人口正义差异：组1均值-组0均值
    grp0 = pred[s == 0]
    grp1 = pred[s == 1]
    if len(grp0) == 0 or len(grp1) == 0:
        return 0.0
    return grp1.mean() - grp0.mean()


def loss_ferm(
    X: np.ndarray,
    y: np.ndarray,
    s: np.ndarray,
    pred: np.ndarray,
    threshold: float = 0.5,
) -> float:  # equalized odds：P(Ŷ=1|A,Y=y)
    yhat = (pred >= threshold).astype(np.float64)

    terms = []
    # y = 1 条件
    mask_y1_g0 = (s == 0) & (y == 1)
    mask_y1_g1 = (s == 1) & (y == 1)
    if mask_y1_g0.sum() > 0 and mask_y1_g1.sum() > 0:
        terms.append(yhat[mask_y1_g0].mean() - yhat[mask_y1_g1].mean())
    # y = 0 条件
    mask_y0_g0 = (s == 0) & (y == 0)
    mask_y0_g1 = (s == 1) & (y == 0)
    if mask_y0_g0.sum() > 0 and mask_y0_g1.sum() > 0:
        terms.append(yhat[mask_y0_g0].mean() - yhat[mask_y0_g1].mean())

    if not terms:
        return 0.0
    return float(np.mean(terms))  # 对可用条件的平均差异作为 EO 损失


def grad_dp(
    grad_fn, X: np.ndarray, s: np.ndarray
) -> np.ndarray:  # 人口正义的梯度（对参数）
    grad_grp0 = (
        grad_fn(X[s == 0])[0] if np.any(s == 0) else np.zeros(X.shape[1])
    )  # 组0的总梯度（对 p 的梯度求和）
    grad_grp1 = (
        grad_fn(X[s == 1])[0] if np.any(s == 1) else np.zeros(X.shape[1])
    )  # 组1的总梯度
    denom0 = max(1, np.sum(s == 0))  # 组0样本数（避免除0）
    denom1 = max(1, np.sum(s == 1))  # 组1样本数
    return (
        grad_grp1 / denom1 - grad_grp0 / denom0
    )  # 两组平均梯度之差：E[∇p|A=1]-E[∇p|A=0]


def grad_ferm(
    grad_fn, X: np.ndarray, y: np.ndarray, s: np.ndarray
) -> np.ndarray:  # equalized odds：对 E[p] 的可微近似梯度
    # grad_fn 返回 (total_grad, indiv_grad)，其中 indiv_grad ≈ ∂p/∂w
    total_grad, indiv_grad = grad_fn(X)  # 计算总梯度与个体梯度（对概率 p 的参数梯度）

    grads = []  # 用于收集各条件(y=1,y=0)下两组平均梯度的差
    # y = 1 条件（对应 TPR 的可微近似）
    mask_y1_g0 = (s == 0) & (y == 1)  # 选取组 A=0 且标签 y=1 的样本
    mask_y1_g1 = (s == 1) & (y == 1)  # 选取组 A=1 且标签 y=1 的样本
    if mask_y1_g0.sum() > 0 and mask_y1_g1.sum() > 0:  # 两组在该条件下都非空才计算差异
        avg_g0_y1 = indiv_grad[mask_y1_g0].mean(axis=0)  # 组 A=0 在 y=1 的平均个体梯度
        avg_g1_y1 = indiv_grad[mask_y1_g1].mean(axis=0)  # 组 A=1 在 y=1 的平均个体梯度
        grads.append(avg_g0_y1 - avg_g1_y1)  # 存入两组平均梯度之差（y=1 条件）
    # y = 0 条件（对应 FPR 的可微近似）
    mask_y0_g0 = (s == 0) & (y == 0)  # 选取组 A=0 且标签 y=0 的样本
    mask_y0_g1 = (s == 1) & (y == 0)  # 选取组 A=1 且标签 y=0 的样本
    if mask_y0_g0.sum() > 0 and mask_y0_g1.sum() > 0:  # 两组在该条件下都非空才计算差异
        avg_g0_y0 = indiv_grad[mask_y0_g0].mean(axis=0)  # 组 A=0 在 y=0 的平均个体梯度
        avg_g1_y0 = indiv_grad[mask_y0_g1].mean(axis=0)  # 组 A=1 在 y=0 的平均个体梯度
        grads.append(avg_g0_y0 - avg_g1_y0)  # 存入两组平均梯度之差（y=0 条件）

    if not grads:  # 若两个条件都不可用（例如某组样本缺失），返回全零梯度
        return np.zeros(X.shape[1])  # 维度与参数维度一致的零向量
    return np.mean(grads, axis=0)  # 对可用条件的组差取均值，得到最终 EO 梯度


def fit_reweight_predictor(
    X_train: np.ndarray,
    y_train: np.ndarray,
    A_train: np.ndarray,
    X_val: np.ndarray,
    y_val: np.ndarray,
    A_val: np.ndarray,
    mode: str,
    lam: float = 0.5,
    alpha: float = 0.1,
    beta: float = 1.0,
    gamma: float = 1.0,
    pretrain_epochs: int = 30,
    pretrain_lr: float = 1e-3,
    pretrain_batch_size: int = 250,
    finetune_epochs: int = 100,
    finetune_lr: float = 5e-3,
    finetune_batch_size: int = 250,
    experiments: int = 1,
    random_state: Optional[int] = None,
) -> nn.Module:  # 便捷函数：直接训练 + 导出 PyTorch 预测器
    metric = "eo" if mode == "eo" else "dp"
    # 确保 alpha 在有效范围内
    alpha = float(np.clip(alpha, 0.0, 1.0))
    trainer = ReweightTrainer(
        metric=metric,
        alpha=alpha,
        beta=beta,
        gamma=gamma,
        pretrain_epochs=pretrain_epochs,
        pretrain_lr=pretrain_lr,
        pretrain_batch_size=pretrain_batch_size,
        finetune_epochs=finetune_epochs,
        finetune_lr=finetune_lr,
        finetune_batch_size=finetune_batch_size,
        experiments=experiments,
        random_state=random_state,
    )
    trainer.fit(X_train, y_train, A_train, X_val, y_val, A_val)
    return trainer.build_predictor()
