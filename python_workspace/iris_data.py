
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from scipy import stats
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm

path = "./data/Iris.csv"

df = pd.read_csv(path)

# print(df)

plt.scatter(df['PetalLengthCm'], df['PetalWidthCm'])

# plt.legend(df['Species'])

# plt.show()

print(df['Species'])

# 分散分析を実行するための式を定義
formula = 'PetalLengthCm ~ PetalWidthCm * Species'
model = ols(formula, data = df).fit()

# 分散分析の結果を計算
anova_table = anova_lm(model, typ = 2)

print(anova_table)
