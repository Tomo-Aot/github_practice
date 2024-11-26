# 冗長性分析(RDA)
# パッケージ
library(tidyverse)

library(ggpubr)
library(showtext)

library(magick)


# data preparation
fruit = read_csv("./data/fruit_harvest.csv")
env = read_csv("./data/city_weather.csv")

# 気象データを上書き
env = 
  env |> 
  mutate(year = factor(year)) |> 
  group_by(year, City) |> 
  summarise(
    mean_temp = mean(mean_temp),
    day_temp_abv30 = sum(day_temp_abv30),
    rain_sum = sum(rain),
    rain_mean = mean(rain),
    sun_time = mean(sun_time),
    .groups = "drop"
  )


fruit |> 
  select(-c(Area, harvest)) |> 
  pivot_wider(
    names_from = City,
    values_from = weight_per_10a
  )

