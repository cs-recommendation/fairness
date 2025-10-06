import numpy as np
import torch
import torch.nn as nn
from scipy.optimize import linprog
from sklearn.linear_model import LogisticRegression
from typing import Optional


class ReweightTrainer:
    def __init__(
        self,
        metric: str = "dp",
        alpha: float = 0.1,
        beta: float = 1.0,
        gamma: float = 1.0,
        l2_reg: float = 1.0,
        random_state: Optional[int] = None,
    ):
        if metric not in {"dp", "eop"}:
            raise ValueError(f"Unsupported metric: {metric}")
        self.metric = metric
        self.alpha = alpha
        self.beta = beta
        self.gamma = gamma
        self.random_state = random_state
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
        self.weights_: Optional[np.ndarray] = None

    def fit(
        self,
        X_train: np.ndarray,
        y_train: np.ndarray,
        A_train: np.ndarray,
        X_val: np.ndarray,
        y_val: np.ndarray,
        A_val: np.ndarray,
    ) -> None:
        self.model.fit(X_train, y_train)
        prob_val = self.model.predict_proba(X_val)[:, 1]
        prob_train = self.model.predict_proba(X_train)[:, 1]

        if self.metric == "eop":
            fair_loss_val = loss_ferm(self._log_loss, X_val, y_val, A_val)
        else:
            fair_loss_val = loss_dp(X_val, A_val, prob_val)

        util_grad_total, util_grad_indiv = self._grad(X_val, y_val, prob_val)
        if self.metric == "eop":
            fair_grad_total = grad_ferm(self._grad_wrapper, X_val, y_val, A_val)
        else:
            fair_grad_total = grad_dp(self._grad_pred_wrapper, X_val, A_val)

        hessian = self._hess(X_train, prob_train)
        util_grad_hvp = self._solve_linear(hessian, util_grad_total)
        fair_grad_hvp = self._solve_linear(hessian, fair_grad_total)

        _, train_indiv_grad = self._grad(X_train, y_train, prob_train)
        util_influence = train_indiv_grad @ util_grad_hvp
        fair_influence = train_indiv_grad @ fair_grad_hvp

        removal_prob = self._solve_lp(fair_influence, util_influence, fair_loss_val)
        weights = np.clip(1.0 - removal_prob, 0.0, 1.0)
        self.weights_ = weights.astype(np.float64)

        self.model.fit(X_train, y_train, sample_weight=self.weights_)

    def build_predictor(self) -> nn.Module:
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
    ) -> np.ndarray:
        num_sample = fair_infl.shape[0]
        bounds = [(0.0, 1.0)] * num_sample
        all_one = np.ones(num_sample)

        max_fair = fair_infl[fair_infl < 0.0].sum() if np.any(fair_infl < 0.0) else 0.0
        max_util = util_infl[util_infl < 0.0].sum() if np.any(util_infl < 0.0) else 0.0

        if fair_loss >= -max_fair:
            c = fair_infl.astype(np.float64)
            A_ub = np.vstack(
                [
                    util_infl.reshape(1, -1),
                    all_one.reshape(1, -1),
                ]
            )
            b_ub = np.array([0.0, self.alpha * num_sample], dtype=np.float64)
        else:
            c = all_one.astype(np.float64)
            A_ub = np.vstack(
                [
                    fair_infl.reshape(1, -1),
                    util_infl.reshape(1, -1),
                ]
            )
            b_ub = np.array(
                [
                    self.beta * (-fair_loss),
                    self.gamma * max_util,
                ],
                dtype=np.float64,
            )

        res = linprog(c=c, A_ub=A_ub, b_ub=b_ub, bounds=bounds, method="highs")
        if res.success and res.x is not None:
            return res.x.astype(np.float64)
        return np.zeros(num_sample, dtype=np.float64)

    def _log_loss(
        self,
        X: np.ndarray,
        y: np.ndarray,
        sample_weight: Optional[np.ndarray] = None,
        eps: float = 1e-12,
    ) -> float:
        prob = self.model.predict_proba(X)[:, 1]
        sample_weight = np.ones_like(y) if sample_weight is None else sample_weight
        loss_vec = -(y * np.log(prob + eps) + (1.0 - y) * np.log(1.0 - prob + eps))
        return np.sum(sample_weight * loss_vec)

    def _grad(self, X: np.ndarray, y: np.ndarray, prob: np.ndarray):
        diff = prob - y
        indiv_grad = X * diff[:, None]
        total_grad = indiv_grad.sum(axis=0)
        total_grad += (1.0 / self.model.C) * self.model.coef_.flatten()
        return total_grad, indiv_grad

    def _grad_wrapper(self, X: np.ndarray, y: np.ndarray):
        prob = self.model.predict_proba(X)[:, 1]
        total_grad, indiv = self._grad(X, y, prob)
        return total_grad, indiv

    def _grad_pred_wrapper(self, X: np.ndarray):
        prob = self.model.predict_proba(X)[:, 1]
        factor = prob * (1.0 - prob)
        indiv_grad = X * factor[:, None]
        total_grad = indiv_grad.sum(axis=0)
        return total_grad, indiv_grad

    def _hess(self, X: np.ndarray, prob: np.ndarray):
        factor = prob * (1.0 - prob)
        hess_wo_reg = X.T @ (factor[:, None] * X)
        return hess_wo_reg + (1.0 / self.model.C) * np.eye(X.shape[1])

    @staticmethod
    def _solve_linear(hessian: np.ndarray, vector: np.ndarray) -> np.ndarray:
        try:
            return np.linalg.solve(hessian, vector)
        except np.linalg.LinAlgError:
            hessian_jitter = hessian + np.eye(hessian.shape[0]) * 1e-6
            try:
                return np.linalg.solve(hessian_jitter, vector)
            except np.linalg.LinAlgError:
                return hessian_jitter.T @ vector


def loss_dp(X: np.ndarray, s: np.ndarray, pred: np.ndarray) -> float:
    grp0 = pred[s == 0]
    grp1 = pred[s == 1]
    if len(grp0) == 0 or len(grp1) == 0:
        return 0.0
    return grp1.mean() - grp0.mean()


def loss_ferm(loss_fn, X: np.ndarray, y: np.ndarray, s: np.ndarray) -> float:
    idx_grp0_y1 = (s == 0) & (y == 1)
    idx_grp1_y1 = (s == 1) & (y == 1)
    if idx_grp0_y1.sum() == 0 or idx_grp1_y1.sum() == 0:
        return 0.0
    loss_grp0 = loss_fn(X[idx_grp0_y1], y[idx_grp0_y1])
    loss_grp1 = loss_fn(X[idx_grp1_y1], y[idx_grp1_y1])
    return loss_grp0 / idx_grp0_y1.sum() - loss_grp1 / idx_grp1_y1.sum()


def grad_dp(grad_fn, X: np.ndarray, s: np.ndarray) -> np.ndarray:
    grad_grp0 = grad_fn(X[s == 0])[0] if np.any(s == 0) else np.zeros(X.shape[1])
    grad_grp1 = grad_fn(X[s == 1])[0] if np.any(s == 1) else np.zeros(X.shape[1])
    denom0 = max(1, np.sum(s == 0))
    denom1 = max(1, np.sum(s == 1))
    return grad_grp1 / denom1 - grad_grp0 / denom0


def grad_ferm(grad_fn, X: np.ndarray, y: np.ndarray, s: np.ndarray) -> np.ndarray:
    mask0 = (s == 0) & (y == 1)
    mask1 = (s == 1) & (y == 1)
    if mask0.sum() == 0 or mask1.sum() == 0:
        return np.zeros(X.shape[1])
    grad0 = grad_fn(X[mask0], y[mask0])[0] / mask0.sum()
    grad1 = grad_fn(X[mask1], y[mask1])[0] / mask1.sum()
    return grad0 - grad1


def fit_reweight_predictor(
    X_train: np.ndarray,
    y_train: np.ndarray,
    A_train: np.ndarray,
    X_val: np.ndarray,
    y_val: np.ndarray,
    A_val: np.ndarray,
    mode: str,
    lam: float,
    random_state: Optional[int] = None,
) -> nn.Module:
    metric = "eop" if mode == "eo" else "dp"
    alpha = float(np.clip(lam, 0.0, 1.0))
    trainer = ReweightTrainer(metric=metric, alpha=alpha, random_state=random_state)
    trainer.fit(X_train, y_train, A_train, X_val, y_val, A_val)
    return trainer.build_predictor()
