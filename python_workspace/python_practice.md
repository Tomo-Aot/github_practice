# Python Practice Workspace
ここには、 Python
の習得状況や作成したスクリプトファイルを共有していきます。
ここでは、基本的な操作から練習していきます。
最終的な目標は、 Python で一般化線形モデルを作成して、作図することです。
また、 web アプリの開発も挑戦してみたいので、その内容も共有していきます。

### 開発環境
OS ... Ubuntu 24.04 LTS

Version ... 3.13

Text Editor ... Visual Studio Code

## パッケージ

データの読み込み、可視化、解析に必要なパッケージを読み込みます。

```
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from scipy import stats
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
```

