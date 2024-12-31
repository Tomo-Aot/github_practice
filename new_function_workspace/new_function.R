# 2024-12-31
# New function workspace
# packages
library(tidyverse)
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
    aes(x = cos(x), y = sin(y))
  )





