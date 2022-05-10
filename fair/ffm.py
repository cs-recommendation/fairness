import numpy as np

np.random.seed(0)
import math
from logistic import Logistic


class FFM_Node(object):
    __slots__ = ['j', 'f', 'v']

    def __init__(self, j, f, v):
        self.j = j
        self.f = f
        self.v = v


class FFM(object):
    def __init__(self, m, n, k, eta, lambd):
        self.m = m
        self.n = n
        self.k = k
        self.eta = eta
        self.lambd = lambd
        self.w = np.random.rand(n, m, k) / math.sqrt(k)
        self.G = np.ones(shape=(n, m, k), dtype=np.float64)
        self.log = Logistic()

    def phi(self, node_list):
        z = 0.0
        for a in range(len(node_list)):
            if a < 19:
                node1 = node_list[a]
                j1 = node1.j
                f1 = node1.f
                v1 = node1.v
                for b in range(a + 1, len(node_list)):
                    node2 = node_list[b]
                    j2 = node2.j
                    f2 = node2.f
                    v2 = node2.v
                    w1 = self.w[j1, f2]
                    w2 = self.w[j2, f1]
                    z += np.dot(w1, w2) * v1 * v2
            else:
                break

        return z

    def predict(self, node_list):
        z = self.phi(node_list)
        y = self.log.decide_by_tanh(z)
        return y

    def sgd(self, node_list, y):
        kappa = -y / (1 + math.exp(y * self.phi(node_list)))
        for a in range(len(node_list)):
            node1 = node_list[a]
            j1 = node1.j
            f1 = node1.f
            v1 = node1.v
            for b in range(a + 1, len(node_list)):
                node2 = node_list[b]
                j2 = node2.j
                f2 = node2.f
                v2 = node2.v
                c = kappa * v1 * v2
                g_j1_f2 = self.lambd * self.w[j1, f2] + c * self.w[j2, f1]
                g_j2_f1 = self.lambd * self.w[j2, f1] + c * self.w[j1, f2]
                self.G[j1, f2] += g_j1_f2 ** 2
                self.G[j2, f1] += g_j2_f1 ** 2
                self.w[j1, f2] -= self.eta / np.sqrt(self.G[j1, f2]) * g_j1_f2
                self.w[j2, f1] -= self.eta / np.sqrt(
                    self.G[j2, f1]) * g_j2_f1

    def train(self, sample_generator, max_echo, max_r2):
        for itr in range(max_echo):
            y_sum = 0.0
            y_square_sum = 0.0
            err_square_sum = 0.0
            population = 0
            for node_list, y in sample_generator:
                y = 0 if y == -1 else y
                self.sgd(node_list, y)
                y_hat = self.predict(node_list)
                y_sum += y
                y_square_sum += y ** 2
                err_square_sum += (y - y_hat) ** 2
                population += 1
            var_y = y_square_sum - y_sum * y_sum/population
            r2 = 1 - err_square_sum / var_y
            if r2 > max_r2:
                break

    def save_model(self, outfile):
        np.save(outfile, self.w)

    def load_model(self, infile):
        self.w = np.load(infile)

    def update_model(self, file, weights):
        temp_weights = np.array(weights)
        for l in range(len(temp_weights)):
            temp_weights[l]=(temp_weights[l]-temp_weights.min())/(temp_weights.max()-temp_weights.min())
        self.w = np.load(file)
        for i in range(self.w.shape[0]):
            for j in range(self.w.shape[1]):
                if temp_weights[j] != 0.0:
                    for k in range(self.w.shape[2]):
                        self.w[i][j][k] += temp_weights[j]





