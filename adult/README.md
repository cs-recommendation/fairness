# 公平性机器学习算法测试框架


### 运行第一个实验

```bash
# 最简单的使用方式 - 使用默认参数
python main.py

# 指定方法和数据集
python main.py --method mixup --dataset adult --mode eo

# 进行超参数搜索
python main.py --method GapReg --dataset adult --search_hp
```

## 🎯 支持的算法

| 算法 | 说明 | 适用场景 |
|------|------|----------|
| `erm` | 经验风险最小化（基线） | 不考虑公平性的标准训练 |
| `mixup` | Fair Mixup | 通过样本混合实现公平性 |
| `GapReg` | Gap 正则化 | 通过惩罚群体间差距实现公平 |
| `fliprate` | 高影响样本方法 | 利用 fliprate 选择关键样本 |
| `adversarial` | 对抗去偏 | 使用对抗网络消除偏见 |
| `reweight` | 影响函数重加权 | 基于影响函数的样本加权 |

### 公平性模式

- **DP (Demographic Parity)**: 人口统计均等，要求不同敏感属性群体的预测结果分布相同
- **EO (Equalized Odds)**: 机会均等，要求在不同敏感属性群体中真阳率和假阳率相同

## 📊 数据集

### 内置数据集

1. **Adult Income Dataset** (`adult`)
   - 48,842 个样本
   - 二分类任务：预测收入是否 >50K
   - 敏感属性：性别

2. **Synthetic Datasets** (`synthetic_k`)
   - 参数化生成的合成数据
   - k 值范围：0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4
   - 用于控制实验，研究不同偏见程度的影响


### 数据集使用

```bash
# Adult 数据集
python main.py --method mixup --dataset adult

# 单个 Synthetic 数据集
python main.py --method GapReg --dataset synthetic_1.5

# 批量测试多个 k 值
python main.py --method mixup --k_values 1 2 3 4
```

## 📖 使用指南

### 单次运行

最简单的使用方式，快速测试单个配置：

```bash
# 基本使用
python main.py --method mixup --dataset adult --mode eo

# 启用超参数搜索
python main.py --method GapReg --dataset synthetic_1 --search_hp

# 使用预处理
python main.py --method erm --dataset adult --preprocess fairshift
```

### 批量运行

系统会自动识别批量运行模式（提供多个方法/数据集/预处理时）：

```bash
# 比较多个方法
python main.py --methods erm mixup GapReg --dataset adult --mode eo --search_hp

# 在多个数据集上测试
python main.py --method GapReg --k_values 1 2 3 4 --mode eo --search_hp

# 完整实验矩阵
python main.py --methods erm mixup GapReg fliprate \
               --k_values 1 1.5 2 2.5 3 \
               --preprocess_list "" fairshift \
               --mode eo --search_hp
```

### 核心参数说明

| 参数 | 说明 | 默认值 | 示例 |
|------|------|--------|------|
| `--method` / `--methods` | 训练方法（单个/多个） | mixup | `--method mixup` |
| `--dataset` | 数据集选择 | adult | `--dataset synthetic_1.5` |
| `--k_values` | Synthetic 数据集的 k 值列表 | - | `--k_values 1 2 3` |
| `--mode` | 公平性模式 (dp/eo) | dp | `--mode eo` |
| `--preprocess` / `--preprocess_list` | 预处理方法 | "" | `--preprocess fairshift` |
| `--search_hp` | 是否进行超参数搜索 | False | `--search_hp` |
| `--hp_selection_metric` | 超参选择指标 (gap/ap) | gap | `--hp_selection_metric gap` |
| `--num_exp` | 实验重复次数 | 5 | `--num_exp 10` |
| `--model_arch` | 模型架构 (mlp/logistic) | mlp | `--model_arch mlp` |
| `--output_csv` | 结果保存路径 | 自动生成 | `--output_csv my_results.csv` |

## ⚙️ 超参数配置

### 配置文件

所有方法的超参数配置存储在 `hyper_parameter_list` 文件中（CSV 格式）：

| 列名 | 说明 |
|------|------|
| method | 方法名称 |
| param_name | 参数名称 |
| param_type | 参数类型 (float/int) |
| default_value | 默认值 |
| search_space | 搜索空间（Python 列表格式） |
| search_step | 搜索步长（预留） |

### 修改超参数

直接编辑 `hyper_parameter_list` 文件：

```csv
# 修改 mixup 的 lambda 参数搜索空间
mixup,lam,float,0.5,"[0.1, 0.3, 0.5, 0.7, 0.9]",0.1

# 修改学习率
mixup,lr,float,0.001,"[0.0001, 0.001, 0.01]",0.001
```

### 超参数搜索策略

```bash
# 使用 gap 作为选择指标（推荐用于公平性优先）
python main.py --method GapReg --dataset adult --search_hp --hp_selection_metric gap

# 使用 ap 作为选择指标（推荐用于准确率优先）
python main.py --method mixup --dataset adult --search_hp --hp_selection_metric ap
```

