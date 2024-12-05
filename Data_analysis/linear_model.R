# 2024-12-05
# toomoyuki aota
# packages
library(tidyverse)
library(stats)
library(patchwork)
library(ggpubr)
library(showtext)
library(magick)
library(sysfonts)

# フォントの埋め込み
font_add_google(name = "Noto Sans JP", family = "ns")
theme_pubr(base_size = 10, base_family = "ns") |> 
  theme_set()
showtext_auto()

# 画像の設定
height = 80
width = height

# データの準備
df = iris |> as_tibble()

model = lm(formula = Petal.Length ~ Petal.Width,
           data =df)

model2 = glm(formula = Petal.Length ~ Petal.Width,
           data =df)

# 変数に値を入力するとある値を出力する関数
# 一般線形モデルでも一般化線形モデルでも使えます
y = function(model, x){
  intercept = model$coefficient[1] # y切片
  coefficient = model$coefficients[2] # 係数
  return(x * coefficient + intercept) # 出力値
}

# Petal.LengthとPetal.Widthでモデルを作成します
# 正規分布を仮定した一般化線形モデルと
# ガンマ分布を仮定した一般化線形モデルを作成します
# 初めに、変数名が長いのでPL,PWに名前を変える
df = df |> 
  select(PL = Petal.Length, 
         PW = Petal.Width,
         Species)

# モデルの作成
# モデルを作成する際は、ヌルモデル(帰無仮説の定義)も作ります
m0 = glm(formula = PW ~ 1, data = df, family = gaussian("identity"))
m1 = glm(formula = PW ~ PL + Species, data = df, family = gaussian("identity"))
m2 = glm(formula = PW ~ 1, data = df, family = Gamma("log"))
m3 = glm(formula = PW ~ PL + Species, data = df, family = Gamma("log"))
# 正規分布の一般化線形モデルは一般線形モデルと同じです。
# lm()で作成しても同じです

# 赤池情報量規準(AIC)の計算
AIC(m0, m1, m2, m3)

# モデルの診断図
plot(m0, which = 1)
plot(m1, which = 1)
plot(m2, which = 1)
plot(m3, which = 1)

# 診断図の結果は、帰無モデルはこのデータを説明しない
# そのため、説明変数に、花弁の長さを入れたほうが良い
# 種の相互作用を加えたモデルの場合、
# m1とm3の診断図では、m1で等分散性が示されたが、
# m3では、示されなかった。
# このデータの場合、正規分布を仮定した一般化線形モデルが
# 最も説明した。

# 正規分布
pdata = df |> 
  group_by(Species) |> 
  expand(
    PL = seq(min(PL), max(PL), length = 21)
  )

pdata = predict(m1, newdata = pdata, se.fit = TRUE) |> 
  bind_cols(pdata)

p1 = df |> 
  ggplot() + 
  geom_point(
    aes(x = PL, y = PW, colour = Species)
  ) + 
  geom_line(
    aes(x = PL, y = fit, colour = Species),
    data = pdata
  ) + 
  geom_ribbon(
    aes(x = PL, 
        ymin = fit - se.fit, 
        ymax = fit + se.fit, 
        fill = Species),
    alpha = 0.4,
    data = pdata
  ) + 
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.direction = "vertical"
  ) + 
  scale_fill_viridis_d(end = 0.8) + 
  scale_colour_viridis_d(end = 0.8)

# ガンマ分布
pdata = df |> 
  group_by(Species) |> 
  expand(
    PL = seq(min(PL), max(PL), length = 21)
  )

pdata = predict(m3, newdata = pdata, se.fit = TRUE) |> 
  bind_cols(pdata)

p2 = df |> 
  ggplot() + 
  geom_point(
    aes(x = PL, y = PW, colour = Species)
  ) + 
  geom_line(
    aes(x = PL, y = exp(fit), colour = Species),
    data = pdata
  ) + 
  geom_ribbon(
    aes(x = PL, 
        ymin = exp(fit - se.fit), 
        ymax = exp(fit + se.fit), 
        fill = Species),
    alpha = 0.4,
    data = pdata
  ) + 
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.direction = "vertical"
  ) + 
  scale_fill_viridis_d(end = 0.8) + 
  scale_colour_viridis_d(end = 0.8)

p1 + p2 + plot_layout(nrow = 1)

# save image
# pdfname = "./image/Generalized_linear_model.pdf"
# pngname = str_replace(pdfname, pattern = "pdf", replacement = "png")
# 
# ggsave(filename = pdfname, height = height, 
#        width = width * 2, units = "mm")
# 
# image_read_pdf(pdfname, density = 600) |> 
#   image_write(pngname)

