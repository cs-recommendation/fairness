import numpy as np  # 数值计算库
import torch  # PyTorch 主库
import torch.nn as nn  # 神经网络模块（用于导出预测器）
from scipy.optimize import linprog  # 线性规划求解器（用于样本删除/重加权）
from typing import Optional  # 可选类型注解
from sklearn.linear_model import LogisticRegression  # 逻辑回归基模型


class ReweightTrainer:  # 重加权训练器：基于影响函数 + 线性规划
    def __init__(
        self,
        metric: str = "dp",
        alpha: float = 0.1,
        beta: float = 1.0,
        gamma: float = 1.0,
        l2_reg: float = 1.0,
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
        self.model = LogisticRegression(
            penalty="l2",
            C=1.0 / l2_reg,
            fit_intercept=False,
            tol=1e-8,
            solver="lbfgs",
            max_iter=2048,
        )
        self.weights_: Optional[np.ndarray] = None  # 最终得到的样本权重（1-删除概率）

    def fit(
        self,
        X_train: np.ndarray,
        y_train: np.ndarray,
        A_train: np.ndarray,
        X_val: np.ndarray,
        y_val: np.ndarray,
        A_val: np.ndarray,
    ) -> None:
        # 使用逻辑回归拟合
        self.model.fit(X_train, y_train)
        prob_val = self.model.predict_proba(X_val)[:, 1]
        prob_train = self.model.predict_proba(X_train)[:, 1]

        if self.metric == "eo":  # Equalized Odds：在 y∈{0,1} 条件下比较两组正预测率
            fair_loss_val = loss_ferm(X_val, y_val, A_val, prob_val)
        else:  # 人口正义：两组平均预测概率之差
            fair_loss_val = loss_dp(X_val, A_val, prob_val)

        util_grad_total, util_grad_indiv = self._grad(
            X_val, y_val, prob_val
        )  # 效用梯度
        if self.metric == "eo":
            fair_grad_total = grad_ferm(self._grad_pred_wrapper, X_val, y_val, A_val)
        else:
            fair_grad_total = grad_dp(self._grad_pred_wrapper, X_val, A_val)

        hessian = self._hess(X_train, prob_train)
        util_grad_hvp = self._solve_linear(hessian, util_grad_total)  # H^{-1} g_util
        fair_grad_hvp = self._solve_linear(hessian, fair_grad_total)  # H^{-1} g_fair

        _, train_indiv_grad = self._grad(X_train, y_train, prob_train)
        util_influence = (
            train_indiv_grad @ util_grad_hvp
        )  # 每个训练样本对效用的影响估计（通过HVP算子）
        fair_influence = (
            train_indiv_grad @ fair_grad_hvp
        )  # 每个训练样本对公平的影响估计（通过HVP算子）

        removal_prob = self._solve_lp(
            fair_influence, util_influence, fair_loss_val
        )  # 求解最优“删除概率”
        weights = np.clip(1.0 - removal_prob, 0.0, 1.0)  # 转换为样本权重（保留概率）
        self.weights_ = weights.astype(np.float64)  # 存储权重

        # 使用权重重新拟合逻辑回归
        self.model.fit(X_train, y_train, sample_weight=self.weights_)

    def build_predictor(
        self,
    ) -> nn.Module:  # 将 sklearn 权重映射到 PyTorch 模块以统一推理接口
        weight = self.model.coef_.astype(np.float32)
        predictor = nn.Linear(weight.shape[1], 1, bias=False)
        with torch.no_grad():
            predictor.weight.copy_(torch.from_numpy(weight))
        for param in predictor.parameters():
            param.requires_grad_(False)
        sequential = nn.Sequential(predictor, nn.Sigmoid())
        device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        sequential.to(device)
        sequential.eval()
        return sequential

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

    # 兼容旧接口：以下方法用于 LR 版本
    def _log_loss(
        self,
        X: np.ndarray,
        y: np.ndarray,
        sample_weight: Optional[np.ndarray] = None,
        eps: float = 1e-12,
    ) -> float:  # 对数损失（可带样本权重）
        prob = self.model.predict_proba(X)[:, 1]
        sample_weight = np.ones_like(y) if sample_weight is None else sample_weight
        loss_vec = -(y * np.log(prob + eps) + (1.0 - y) * np.log(1.0 - prob + eps))
        return np.sum(sample_weight * loss_vec)

    def _grad(
        self, X: np.ndarray, y: np.ndarray, prob: np.ndarray
    ):  # 逻辑回归的梯度（含 L2 正则）
        diff = prob - y
        indiv_grad = X * diff[:, None]
        total_grad = indiv_grad.sum(axis=0)
        total_grad += (1.0 / self.model.C) * self.model.coef_.flatten()
        return total_grad, indiv_grad

    def _grad_wrapper(
        self, X: np.ndarray, y: np.ndarray
    ):  # 包装：先预测概率，再调 _grad
        prob = self.model.predict_proba(X)[:, 1]
        total_grad, indiv = self._grad(X, y, prob)
        return total_grad, indiv

    def _grad_pred_wrapper(self, X: np.ndarray):  # 仅依赖预测概率的梯度（用于 dp）
        prob = self.model.predict_proba(X)[:, 1]
        factor = prob * (1.0 - prob)
        indiv_grad = X * factor[:, None]
        total_grad = indiv_grad.sum(axis=0)
        return total_grad, indiv_grad

    def _hess(
        self, X: np.ndarray, prob: np.ndarray
    ):  # 逻辑回归 Hessian 近似（含 L2 正则）
        factor = prob * (1.0 - prob)
        hess_wo_reg = X.T @ (factor[:, None] * X)
        return hess_wo_reg + (1.0 / self.model.C) * np.eye(X.shape[1])

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
    random_state: Optional[int] = None,
) -> nn.Module:  # 便捷函数：直接训练 + 导出 PyTorch 预测器
    metric = "eo" if mode == "eo" else "dp"
    # 确保 alpha 在有效范围内
    alpha = float(np.clip(alpha, 0.0, 1.0))
    trainer = ReweightTrainer(
        metric=metric, alpha=alpha, beta=beta, gamma=gamma, random_state=random_state
    )
    trainer.fit(X_train, y_train, A_train, X_val, y_val, A_val)
    return trainer.build_predictor()
