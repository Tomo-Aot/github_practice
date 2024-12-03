# Data analysis Workspace

## Principal Component Analysis (PCA)

```
# パッケージの読み込み
library(tidyverse) # 作図用
library(stats) # 主成分分析用
library(vegan) # 非類似度の解析に使います
library(ggpubr) # 作図用
library(showtext) # フォント用
library(magick) # 図の保存用

# フォントはGoogleのNoto Sans、フォントサイズは10ptに設定します
font_add_google(name = "Noto Sans", family = "ns")
theme_pubr(base_family = "ns", base_size = 10) |> 
  theme_set()
showtext_auto()

# 保存する図のサイズを指定します
# 80mm * 80mmにしておくと論文に掲載する際に便利です
height = 80
width = height

# そのままだと、それぞれの変数の形式がわからないので、tibbleにします
df = iris |> as_tibble()

# 行列にする際に、データを標準化します
mat = scale(df[,c(1:4)], center = TRUE, scale = TRUE)

# 主成分分析の結果
result = prcomp(mat)

# 要約統計量の確認
summary(result)

# 主成分をtibbleにして元のデータの種名を加えます
pc = result$x |> 
  as_tibble() |> 
  mutate(Species = df$Species)

# 矢印を追加します
# 矢印の大きさは主成分分析のRotationの係数で決まります
rot = result$rotation

# 各主成分の寄与率を計算して、図の軸に表示します
# 寄与率の確認方法がわからないので、計算します
ctb = function(x){
  sdev = x
  
  # 各主成分の分散
  variance = sdev ^ 2
  # 総分散
  total_variance = sum(variance)
  
  # 各主成分の寄与率（%）
  contribution = variance / total_variance * 100
  
  return(contribution)
}

# 寄与率と主成分軸をまとめます
per = 
  ctb(result$sdev) |> 
  as_tibble() |> 
  mutate(PC = colnames(result$x))

# 最終的な図は次のようになる
# 今回は寄与率の整数部分だけ表示します
xlab = str_c("PC1 (", floor(per[1, 1]), "%)")
ylab = str_c("PC2 (", floor(per[2, 1]), "%)")

# デフォルトだと矢印が小さくて見づらいので、
# 2.5 倍にします
scale_rot = 2.5

pc |> 
  ggplot() + 
  geom_point(
    aes(x = PC1, y = PC2, colour = Species)
  ) + 
  geom_segment(
    aes(x = 0, y = 0,
        xend = PC1 * scale_rot, yend = PC2 * scale_rot),
    data = rot,
    arrow = arrow(type = "open", length = unit(1, "mm"), angle = 20),
    size = 0.5
  ) + 
  scale_x_continuous(
    name = xlab,
    limits = c(-3.5, 4.5),
    breaks = seq(-3.5, 4.5, length = 5)
  ) + 
  scale_y_continuous(
    name = ylab,
    limits = c(-3, 4),
    breaks = seq(-3, 4, length = 6)
  ) + 
  theme(
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.direction = "vertical",
    panel.border = element_rect(size = 1, fill = NA)
  ) + 
  scale_colour_viridis_d(end = 0.8)
```
