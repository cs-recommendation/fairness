import numpy as np
import math
from singleton import Singleton


class Logistic(object):
    __metaclass__ = Singleton

    def __init__(self):
        exp_max = 10.0
        self.exp_scale = 0.001
        self.exp_intv = int(exp_max / self.exp_scale)
        self.exp_table = [0.0] * self.exp_intv
        for i in range(self.exp_intv):
            x = self.exp_scale * i
            exp = math.exp(x)
            self.exp_table[i] = exp / (1.0 + exp)

    def decide_by_table(self, x):
        if x == 0:
            return 0.5
        i = int(np.nan_to_num(abs(x) / self.exp_scale))
        y = self.exp_table[min(i, self.exp_intv - 1)]
        if x > 0:
            return y
        else:
            return 1.0 - y

    def decide_by_tanh(self, x):
        return 0.5 * (1 + np.tanh(0.5 * x))

    def decide(self, x):
        return 1.0 / (1.0 + np.exp(-x))


if __name__ == '__main__':
    log = Logistic()
    for x in np.arange(-20, 20, 0.1):
        y = log.decide(x)
        print(x, y, log.decide_by_tanh(x) - y, log.decide_by_table(x) - y)
