import os
import random
from typing import Optional

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import DataLoader, TensorDataset
from tqdm import tqdm


class _ClassifierModel(nn.Module):
    def __init__(self, feature_dim: int, hidden_units: int, dropout: float):
        super().__init__()
        self.fc1 = nn.Linear(feature_dim, hidden_units)
        self.fc2 = nn.Linear(hidden_units, 1)
        self.dropout = nn.Dropout(p=dropout)

    def forward(self, x):
        hidden = torch.relu(self.fc1(x))
        hidden = self.dropout(hidden)
        logits = self.fc2(hidden)
        probs = torch.sigmoid(logits)
        return probs, logits


class _AdversaryModel(nn.Module):
    def __init__(self, n_groups: int):
        super().__init__()
        # 原论文中用于构建 adversary 输入的缩放参数
        self.c = nn.Parameter(torch.tensor(1.0))
        self.fc = nn.Linear(3, n_groups)

    def forward(self, pred_logits, true_labels):
        scaled = torch.sigmoid((1 + torch.abs(self.c)) * pred_logits)
        features = torch.cat(
            [scaled, scaled * true_labels, scaled * (1.0 - true_labels)], dim=1
        )
        logits = self.fc(features)
        probs = torch.sigmoid(logits)
        return probs, logits


class _ClassifierPredictor(nn.Module):
    def __init__(self, classifier: _ClassifierModel, device: torch.device):
        super().__init__()
        self.classifier = classifier
        self.device = device

    def forward(self, x):
        x = x.to(self.device)
        probs, _ = self.classifier(x)
        return probs


class AdversarialDebiasingTorch:
    """
    将 Zhang 等人的对抗消偏算法适配为当前项目的 PyTorch 训练器。

    该实现支持 numpy 数组输入，适用于当前工程中的 synthetic2 等数据集。
    """

    def __init__(
        self,
        adversary_loss_weight: float = 0.1,
        num_epochs: int = 50,
        batch_size: int = 256,
        classifier_num_hidden_units: int = 200,
        dropout: float = 0.2,
        debias: bool = True,
        verbose: bool = False,
        random_state: Optional[int] = None,
        device: Optional[torch.device] = None,
    ):
        self.adversary_loss_weight = adversary_loss_weight
        self.num_epochs = num_epochs
        self.batch_size = batch_size
        self.classifier_num_hidden_units = classifier_num_hidden_units
        self.dropout = dropout
        self.debias = debias
        self.verbose = verbose
        self.random_state = random_state
        self.device = device or torch.device(
            "cuda" if torch.cuda.is_available() else "cpu"
        )

        self.classifier: _ClassifierModel | None = None
        self.adversary: _AdversaryModel | None = None

    def _set_seed(self, seed: int):
        os.environ["PYTHONHASHSEED"] = str(seed)
        random.seed(seed)
        np.random.seed(seed)
        torch.manual_seed(seed)
        if torch.cuda.is_available():
            torch.cuda.manual_seed_all(seed)

    def fit(self, X: np.ndarray, y: np.ndarray, A: np.ndarray):
        if self.random_state is not None:
            self._set_seed(self.random_state)

        X = torch.tensor(X, dtype=torch.float32)
        y = torch.tensor(y.reshape(-1, 1), dtype=torch.float32)
        A = torch.tensor(A.reshape(-1, 1), dtype=torch.float32)

        dataset = TensorDataset(X, y, A)
        dataloader = DataLoader(dataset, batch_size=self.batch_size, shuffle=True)

        feature_dim = X.shape[1]
        n_groups = int(A.unique().numel())
        n_groups = 1 if n_groups == 2 else n_groups

        self.classifier = _ClassifierModel(
            feature_dim=feature_dim,
            hidden_units=self.classifier_num_hidden_units,
            dropout=self.dropout,
        ).to(self.device)

        classifier_opt = torch.optim.Adam(
            self.classifier.parameters(), lr=1e-3, weight_decay=1e-5
        )

        loss_clf = F.binary_cross_entropy_with_logits

        if self.debias:
            self.adversary = _AdversaryModel(n_groups=n_groups).to(self.device)
            adversary_opt = torch.optim.Adam(
                self.adversary.parameters(), lr=1e-3, weight_decay=1e-5
            )

            # 分类器预训练
            for epoch in tqdm(
                range(max(1, self.num_epochs // 2)),
                disable=not self.verbose,
                desc="Classifier pretrain",
            ):
                self.classifier.train()
                for X_b, y_b, _ in dataloader:
                    X_b = X_b.to(self.device)
                    y_b = y_b.to(self.device)

                    classifier_opt.zero_grad()
                    _, logits = self.classifier(X_b)
                    loss = loss_clf(logits, y_b, reduction="mean")
                    loss.backward()
                    classifier_opt.step()

            # 对抗器预训练
            for epoch in tqdm(
                range(10),
                disable=not self.verbose,
                desc="Adversary pretrain",
            ):
                self.classifier.eval()
                self.adversary.train()
                for X_b, y_b, A_b in dataloader:
                    X_b = X_b.to(self.device)
                    y_b = y_b.to(self.device)
                    A_b = A_b.to(self.device)

                    adversary_opt.zero_grad()
                    _, logits = self.classifier(X_b)
                    _, adv_logits = self.adversary(logits.detach(), y_b)
                    loss = loss_clf(adv_logits, A_b, reduction="mean")
                    loss.backward()
                    adversary_opt.step()

            # 联合训练
            for epoch in tqdm(
                range(self.num_epochs),
                disable=not self.verbose,
                desc="Adversarial training",
            ):
                self.classifier.train()
                self.adversary.train()
                for X_b, y_b, A_b in dataloader:
                    X_b = X_b.to(self.device)
                    y_b = y_b.to(self.device)
                    A_b = A_b.to(self.device)

                    classifier_opt.zero_grad()
                    adversary_opt.zero_grad()

                    probs, logits = self.classifier(X_b)

                    loss_main = loss_clf(logits, y_b, reduction="mean")
                    loss_main.backward(retain_graph=True)

                    clf_grads = [
                        param.grad.detach().clone()
                        for param in self.classifier.parameters()
                    ]

                    classifier_opt.zero_grad()
                    adversary_opt.zero_grad()

                    _, adv_logits = self.adversary(logits, y_b)
                    loss_adv = loss_clf(adv_logits, A_b, reduction="mean")
                    loss_adv.backward()

                    adv_grads = [
                        param.grad.detach().clone()
                        for param in self.classifier.parameters()
                    ]

                    for param, grad_clf, grad_adv in zip(
                        self.classifier.parameters(), clf_grads, adv_grads
                    ):
                        if grad_adv.norm().item() == 0:
                            param.grad = grad_clf
                            continue

                        unit_adv = grad_adv / (
                            grad_adv.norm() + torch.finfo(torch.float32).eps
                        )
                        proj = torch.sum(unit_adv * grad_clf)
                        param.grad = (
                            grad_clf
                            - proj * unit_adv
                            - self.adversary_loss_weight * grad_adv
                        )

                    classifier_opt.step()
                    adversary_opt.step()

        else:
            # 单纯的分类器训练
            for epoch in tqdm(
                range(self.num_epochs),
                disable=not self.verbose,
                desc="Classifier training",
            ):
                self.classifier.train()
                for X_b, y_b, _ in dataloader:
                    X_b = X_b.to(self.device)
                    y_b = y_b.to(self.device)

                    classifier_opt.zero_grad()
                    _, logits = self.classifier(X_b)
                    loss = loss_clf(logits, y_b, reduction="mean")
                    loss.backward()
                    classifier_opt.step()

        return self

    def get_predictor(self) -> nn.Module:
        if self.classifier is None:
            raise RuntimeError("Classifier has not been trained yet.")
        predictor = _ClassifierPredictor(self.classifier, self.device)
        return predictor.to(self.device)
