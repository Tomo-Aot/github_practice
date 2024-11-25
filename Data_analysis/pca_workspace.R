# 2024-11-25
# Aota Tomoyuki
# Data Analysis Workspace
# Rのデータベースにある Iris のデータを使って主成分分析 (PCA) をします

# パッケージの読み込み
library(tidyverse) # 作図用
library(ggpubr) # 作図用
library(showtext) # フォント用
library(magick) # 図の保存用

# 最初に、フォントを埋め込みます
# フォントはGoogleのNoto Sans、フォントサイズは10ptに設定します
font_add_google(name = "Noto Sans", family = "ns")
theme_pubr(base_family = "ns", base_size = 10) |> 
  theme_set()
showtext_auto()

# データを少し変えます
# そのままだと、それぞれの変数の形式がわからないので、tibbleにします
df = iris |> as_tibble()

# ここで、一度作図してみます。
# x軸はPetal.Length、y軸はPetal.Widthにします
df |> 
  ggplot() + 
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, colour = Species)
  ) + 
  theme(
    # 凡例の場所を決めます。
    legend.position = c(0, 1), # 図の中の左上に配置
    legend.justification = c(0, 1), # 重心を凡例の左上に合わせる
    legend.background = element_blank(), # 背景を透明にする(デフォルトは白)
    legend.direction = "vertical" # 凡例を並べる方向を鉛直にする
  ) + 
  scale_colour_viridis_d(end = 0.8) # 0~1の値を入力します。色弱でも見やすい

