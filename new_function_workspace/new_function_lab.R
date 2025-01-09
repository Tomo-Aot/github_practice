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
  value = seq(min(value), max(value), length = 500)
)

y = y |> expand(
  value = seq(min(value), max(value), length = 500)
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

triangle |> 
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

# make gif





