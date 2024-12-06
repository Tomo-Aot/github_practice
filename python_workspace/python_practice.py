# Data analysis in Python script
# 2024-11-30
# statistical analysis

# library
import pandas as pd
from pathlib import Path

file = "city_weather.csv"

# pandas パッケージのread_csv()を使って読み込みます
df = pd.read_csv(file)

print(df)
