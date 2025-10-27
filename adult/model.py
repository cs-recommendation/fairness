import torch
import torch.nn as nn
import torch.nn.functional as F


class LogisticRegression(nn.Module):
    def __init__(self, input_size):
        super(LogisticRegression, self).__init__()
        self.linear = nn.Linear(input_size, 1)

    def forward(self, x):
        x = self.linear(x)
        return torch.sigmoid(x)


class Net(nn.Module):
    def __init__(self, input_size):
        super(Net, self).__init__()
        self.fc1 = nn.Linear(input_size, 200)
        self.fc2 = nn.Linear(200, 200)
        self.fc3 = nn.Linear(200, 1)

    def forward(self, x):
        x = self.fc1(x)
        x = F.relu(x)
        x = self.fc2(x)
        x = F.relu(x)
        x = self.fc3(x)
        return torch.sigmoid(x)


def build_model(model_arch, input_size):
    model_arch = model_arch.lower()
    if model_arch in {"mlp", "net"}:
        return Net(input_size)
    if model_arch in {"logistic", "logreg", "lr"}:
        return LogisticRegression(input_size)
    raise ValueError(f"Unsupported model architecture: {model_arch}")
