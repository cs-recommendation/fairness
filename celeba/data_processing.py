import os
import zipfile
import pandas as pd
import numpy as np
from glob import glob
from tqdm import tqdm
from PIL import Image
import shutil
import pickle

labels_path = 'celeba/list_attr_celeba.txt'
image_path = 'celeba/img_align_celeba/'
split_path = 'celeba/list_eval_partition.txt'

# 正确读取文件，第一行作为列名
labels_df = pd.read_csv(labels_path)

# 直接使用pandas读取的数据，不需要手动解析
label_df = labels_df.set_index('image_id')
label_df.replace(['-1'], ['0'], inplace = True)

# generate train/val/test
files = glob(image_path + '*.jpg')

# 读取分割文件（CSV格式）
split_df = pd.read_csv(split_path)

# 如果tmp目录存在则先删除，然后重新创建
if os.path.exists('celeba/tmp/'):
    shutil.rmtree('celeba/tmp/')
os.makedirs('celeba/tmp/')
for i in ['train', 'val', 'test']:
    os.makedirs(os.path.join('celeba/tmp/', i))

train_file_names = []
train_dict = {}
valid_file_names = []
valid_dict = {}
test_file_names = []
test_dict = {}
for i in tqdm(range(len(split_df))):
    file_name = split_df.iloc[i]['image_id']
    sp = str(split_df.iloc[i]['partition'])
    if sp == '0':
        labels = label_df.loc[file_name].values
        train_dict[file_name] = labels
        train_file_names.append(file_name)
        source_path = image_path + file_name
        shutil.copy2(source_path, os.path.join('celeba/tmp/train', file_name))
    elif sp == '1':
        labels = label_df.loc[file_name].values
        valid_dict[file_name] = labels
        valid_file_names.append(file_name)
        source_path = image_path + file_name
        shutil.copy2(source_path, os.path.join('celeba/tmp/val', file_name))
    else:
        labels = label_df.loc[file_name].values
        test_dict[file_name] = labels
        test_file_names.append(file_name)
        source_path = image_path + file_name
        shutil.copy2(source_path, os.path.join('celeba/tmp/test', file_name))

# 创建DataFrame，每行是一个图像，labels列包含标签数组
# 需要将标签数组重新reshape为二维数组格式以兼容原始代码
train_labels = np.array(list(train_dict.values()))
train_df = pd.DataFrame({'labels': [train_labels[i] for i in range(len(train_labels))]}, index=train_file_names)

valid_labels = np.array(list(valid_dict.values()))
valid_df = pd.DataFrame({'labels': [valid_labels[i] for i in range(len(valid_labels))]}, index=valid_file_names)

test_labels = np.array(list(test_dict.values()))
test_df = pd.DataFrame({'labels': [test_labels[i] for i in range(len(test_labels))]}, index=test_file_names)

df = {}
df['train'] = train_df
df['val'] = valid_df
df['test'] = test_df
with open('celeba/data_frame.pickle', 'wb') as handle:
    pickle.dump(df, handle, protocol=pickle.HIGHEST_PROTOCOL)

print('data frame saved')
