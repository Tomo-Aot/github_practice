# 2024-11-25
# Aota Tomoyuki
# Data Analysis Workspace
# Rのデータベースにある Iris のデータを使って主成分分析 (PCA) をします

# パッケージの読み込み
library(tidyverse) # 作図用
library(stats) # 主成分分析用
library(vegan) # 非類似度の解析に使います
library(ggpubr) # 作図用
library(showtext) # フォント用
library(magick) # 図の保存用

# 最初に、フォントを埋め込みます
# フォントはGoogleのNoto Sans、フォントサイズは10ptに設定します
font_add_google(name = "Noto Sans", family = "ns")
theme_pubr(base_family = "ns", base_size = 10) |> 
  theme_set()
showtext_auto()

# 保存する図のサイズを指定します
# 80mm * 80mmにしておくと論文に掲載する際に便利です
height = 80
width = height

# データを少し変えます
# そのままだと、それぞれの変数の形式がわからないので、tibbleにします
df = iris |> as_tibble()

# ここで、一度作図してみます。
# x軸はPetal.Length、y軸はPetal.Widthにします
# 軸ラベルの設定は先に済ませておきます
xlab = "Petal.Length (mm)"
ylab = str_replace(xlab, pattern = "Length", replacement = "Width")

df |> 
  ggplot() + 
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, colour = Species)
  ) + 
  scale_x_continuous(
    name = xlab,
  ) + 
  scale_y_continuous(
    name = ylab,
  ) + 
  theme(
    # 凡例の場所を決めます。
    legend.position = c(0, 1), # 図の中の左上に配置
    legend.justification = c(0, 1), # 重心を凡例の左上に合わせる
    legend.background = element_blank(), # 背景を透明にする(デフォルトは白)
    legend.direction = "vertical" # 凡例を並べる方向を鉛直にする
  ) + 
  scale_colour_viridis_d(end = 0.8) # 0~1の値を入力します。色弱でも見やすい

# 線形モデルが作れそうですが、
# 元のデータフレームに連続変数が 4 つあるので、
# 主成分分析してみます

# 1.データフレームから行列を作成
# 行列にする際に、データを標準化します
mat = scale(df[,c(1:4)], center = TRUE, scale = TRUE)

# 主成分分析の結果
result = prcomp(mat)

result |> 
  ggbiplot::ggbiplot(groups = df$Species) + 
  theme(
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.direction = "vertical"
  ) + 
  scale_colour_viridis_d(end = 0.8)

# 図を保存します。
pdfname = "./image/iris_pca_ggbiplot.pdf"
pngname = str_replace(pdfname, pattern = "pdf", replacement = "png")
ggsave(filename = pdfname, height = height, width = width, units = "mm")
image_read_pdf(pdfname, density = 300) |> 
  image_write(pngname)


# ggplot()で作図します
# 主成分分析の結果から、PC1とPC2で99.9％説明している
pc = result$x |> 
  as_tibble() |> 
  mutate(Species = df$Species)

vector = result$rotation

result$scale

pc |> 
  ggplot() + 
  geom_point(
    aes(x = PC1, y = PC2, colour = Species)
  ) + 
  geom_segment(
    aes(x = 0, y = 0, xend = PC1 * result$scale, yend = PC2 * result$scale),
  ) + 
  scale_colour_viridis_d(end = 0.8)






