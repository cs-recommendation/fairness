import rpy2.robjects as ro
import pandas as pd
import numpy as np
from rpy2.robjects import conversion, pandas2ri
from rpy2.robjects.conversion import localconverter
import math
from ffm import FFM_Node, FFM
import re

def does_response(parameters, sample):
  temp_pars = parameters
  temp_samp = sample
  temp_does = np.zeros(6)
  for index in range(6):
      treat = temp_samp[index]
      tt_samp = temp_samp
      tt_samp[index] = 0
      temp_dnorm = temp_pars[index][0] * tt_samp[0] + temp_pars[index][1] * tt_samp[1] + temp_pars[index][2] * tt_samp[2] + temp_pars[index][3] * \
                   tt_samp[3] + temp_pars[index][4] * tt_samp[4] + temp_pars[index][5] * tt_samp[5] + temp_pars[index][6]
      temp_does[index] = temp_pars[index][7] + temp_pars[index][8] * treat + temp_pars[index][9] * treat * treat + \
                         temp_pars[index][10] * temp_dnorm + temp_pars[index][11] * temp_dnorm * temp_dnorm + \
                         temp_pars[index][12] * treat * temp_dnorm
  return temp_does

class Sample(object):
  def __init__(self, infile):
    self.infile = infile
    self.regex = re.compile("\\s+")

  def __iter__(self):
    with open(self.infile, 'r') as f_in:
      for line in f_in:
        arr = self.regex.split(line.strip())
        if len(arr) >= 2:
          y = float(arr[0])
          assert math.fabs(y) == 1
          node_list = []
          square_sum = 0.0
          for i in range(1, len(arr)):
            brr = arr[i].split(",")
            if len(brr) == 3:
              j = int(brr[0])
              f = int(brr[1])
              v = float(brr[2])
              square_sum += v * v
              node_list.append(FFM_Node(j, f, v))
          if square_sum > 0:
            norm = math.sqrt(square_sum)
            normed_node_list = [FFM_Node(ele.j, ele.f, ele.v / norm) for ele in node_list]
            yield (normed_node_list, y)

if __name__ == '__main__':

  ro.r.source(r'C:/Users/lihon/Desktop/paper/fairness-22/code/data/MC/Generalized-propensity-score-master/GPS_R_codes.R')
  pd_df = pd.read_csv('C:/Users/lihon/Desktop/paper/fairness-22/code/data/MC/gps_200.csv', header=None, sep=",")

  with localconverter(ro.default_converter + pandas2ri.converter):
    xstream = conversion.py2rpy(pd_df)
  x = ro.IntVector(range(1, len(pd_df) + 1))
  dr = ro.r["theta"](x, xstream)
  np_dr0 = np.array(dr[0])

  n = 61
  m = 6
  k = 380
  train_file = "C:/Users/lihon/Desktop/paper/fairness-22/code/data/MC/ffm_train.csv"
  valid_file = "C:/Users/lihon/Desktop/paper/fairness-22/code/data/MC/ffm_validation.csv"
  model_file = "C:/Users/lihon/Desktop/paper/fairness-22/code/fair/ffm.npy"

  eta = 0.0001
  lambd = 1e-4
  max_echo = 300
  max_r2 = 0.9

  sample_generator = Sample(train_file)
  ffm = FFM(m, n, k, eta, lambd)
  ffm.train(sample_generator, max_echo, max_r2)
  ffm.save_model(model_file)

  ffm.load_model(model_file)
  valid_generator = Sample(valid_file)
  y_sum = 0.0
  y_square_sum = 0.0
  err_square_sum = 0.0
  population = 0

  TP = 0
  FP = 0
  TN = 0
  FN = 0
  for node_list, y in valid_generator:
    y = 0.0 if y == -1 else y
    temp_n = np.zeros(6)

    for i in range(6):
      temp_n[i] = node_list[i].v
    temp_d = does_response(np_dr0, temp_n)
    ffm.update_model(model_file, temp_d)
    y_hat = ffm.predict(node_list)
    y_sum += y
    y_square_sum += y ** 2
    err_square_sum += (y - y_hat) ** 2
    population += 1
    if y_hat > 0.5:
      if y == 1: TP += 1
      else: FP += 1
    else:
      if y == 1: FN += 1
      else: TN += 1
  acc = (TP+TN)/(TP+TN+FP+FN)
  print("accuracy is", acc)
  for l in range(len(temp_d)):
    temp_d[l] = (temp_d[l] - temp_d.min()) / (temp_d.max() - temp_d.min())
  fair=(temp_d[0]+temp_d[1]+temp_d[2]+temp_d[3]+temp_d[4]+temp_d[5])/6
  print("fairness is",fair)













