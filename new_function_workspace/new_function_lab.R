# 2024-12-31
# New function workspace
# packages
library(tidyverse)
library(ggtext)
library(magick)
library(showtext)
library(ggpubr)


# setting fonts
font_add_google(name = "Noto Sans JP", family = "nsjp")
theme_pubr(base_family = "nsjp", base_size = 10) |> 
  theme_set()
showtext_auto()

# make circle
x = c(- pi, pi) |> as_tibble()
y = c(- pi, pi) |> as_tibble()

x = x |> expand(
  value = seq(min(value), max(value), length = 5)
)

y = y |> expand(
  value = seq(min(value), max(value), length = 5)
)

circle = x |> 
  rename(x = value) |> 
  bind_cols(y) |> 
  rename(y = value)

# R = sqrt(x^2 + y^2)
# 三角関数で表現する
circle |> 
  ggplot() + 
  geom_point(
    aes(x = cos(x), y = sin(y),
        colour = x)
  ) + 
  theme(
    legend.position = c(0.5, 0.5),
    legend.background = element_blank(),
    legend.direction = "horizontal"
  ) + 
  scale_colour_viridis_b(end = 1)

# save image
# height = 80
# width = height
# pdfname = "./image/circle.pdf"
# pngname = str_replace(pdfname, "pdf", "png")
# 
# ggsave(pdfname, height = height, width = width,
#        units = "mm")
# image_read_pdf(pdfname, density = 300) %>% 
#   image_write(pngname)
# #########################################################
label = tibble(
  x = -1,
  y = 1,
  label = "Circle"
)

circle |> 
  ggplot() + 
  geom_polygon(
    aes(x = cos(x), y = sin(y)),
  ) + 
  geom_richtext(
    aes(x = x, y = y, label = label),
    data = label,
    fill = NA,
    label.colour = NA,
    vjust = 1,
    hjust = 0
  ) + 
  theme(
    legend.position = "none",
  )

# save image
# height = 80
# width = height
# pdfname = "./image/polygon_circle.pdf"
# pngname = str_replace(pdfname, "pdf", "png")
# 
# ggsave(pdfname, height = height, width = width,
#        units = "mm")
# image_read_pdf(pdfname, density = 300) %>%
#   image_write(pngname)

# triangle
triangle = function(n){
  if(n > 0){
  a = n + 1
  b = n + 2
  c = n + 3
  frame = 
    tibble(
      x = c(a, b, c),
      y = c(2 * a, 3 * b, c)
    )
  print(frame)
  }
  else{
  a = n - 1
  b = n - 2
  c = n - 3
  frame = 
    tibble(
      x = c(a, b, c),
      y = c(5 * a, 2 * b, 4 * c)
    )
  print(frame)
  }
}

triangle(n = 7) |> 
  ggplot() + 
  geom_polygon(
    aes(x = x, y = y)
  )

# save image
# height = 80
# width = height
# pdfname = "./image/polygon_triangle.pdf"
# pngname = str_replace(pdfname, "pdf", "png")
# 
# ggsave(pdfname, height = height, width = width,
#        units = "mm")
# image_read_pdf(pdfname, density = 300) %>%
#   image_write(pngname)

# 任意の三角形を作る関数を作成します。
tr_ang = function(a, b, deg_c, fill){
  # 下の図のような三角形を作図します。
  # 二辺の長さをa, bとします。
  # 二辺の鋏角をangleとします。
  # 　　　 　     (x3, y3)
  # 　　　 　     /\
  # 　　　 　    /  \
  # 　　　 　   /    \
  # 　　　 　b /      \ 
  # 　　　 　 /        \
  # 　　　 　/ angle    \
  # (x1, y1)/____________\(x2, y2)
  # 　　　　       a
  
  # 1点目は原点を基準にします。
  x1 = 0
  y1 = 0
  
  # 2つ目の点は1点目と直線で結んで底辺を作成するため、
  # (a, 0)とします。
  x2 = a
  y2 = 0
  
  # 度数法で入力するのが分かりやすいと思うので、
  # 入力値を度で表して、出力の際に弧度法に直します。
  # 図形を作る際は、弧度法を使うと便利な気がします。
  # 180度のとき、弧度法ではπになります。
  # そのため、pi/180に度数法の時の角度を掛けることで、
  # 度数法から弧度法に置換することができます。
  angle = deg_c * pi / 180
  
  # 最後の点は底辺を基準にangle分回転させることを考えます。
  # このとき、線分の長さを辺bと等しくするため、ｂを掛けます。
  x3 = b * cos(angle)
  y3 = b * sin(angle)
  
  # 3点の座標をデータフレームにまとめる
  df = tibble(
    x = c(x1, x2, x3),
    y = c(y1, y2, y3)
  )
  
  # 軸の範囲を揃えます。
  # 最小値
  min_df = 
    function(df){
      if(min(df$x) > min(df$y)){
        min = min(df$y)
      } else{
        min = min(df$x)
      }
    }
  
  # 最大値
  max_df = 
    function(df){
      if(max(df$x) < max(df$y)){
        max = max(df$y)
      } else{
        max = max(df$x)
      }
      return(max)
    }
  
  # プロット
  p = df |> 
    ggplot() + 
    geom_polygon(
      aes(x = x, y = y),
      fill = fill
    ) + 
    scale_x_continuous(
      limits = c(min_df(df), max_df(df)),
      breaks = seq(min_df(df), max_df(df), length = 6)
    ) + 
    scale_y_continuous(
      limits = c(min_df(df), max_df(df)),
      breaks = seq(min_df(df), max_df(df), length = 6)
    ) + 
    theme(
      axis.title = element_blank(),
      axis.ticks = element_line(colour = "black")
    )
  
  # 出力
  return(p)
}


tr_ang(
  a = 2,
  b = sqrt(3),
  deg_c = 30,
  fill = "black"
)

# save image
height = 80
width = height
pdfname = "./image/triangle.pdf"
pngname = str_replace(pdfname, "pdf", "png")

ggsave(pdfname, height = height, width = width,
       units = "mm")
image_read_pdf(pdfname, density = 300) %>%
  image_write(pngname)

