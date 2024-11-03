# 2024-11-02
# structral equation modeling practice
# packages
library(tidyverse)
library(lavaan)
library(semPlot)
library(semTools)
library(tidySEM)

# prepare data
fish = "./data/All_fish_dataset.rds"
nana_seaweed = "./data/species_writeout_spdf_naname_2021.rds"
ari_seaweed = "./data/species_writeout_spdf_arikawa_2021.rds"
nana_temp = "./data/temp_writeout_naname_2021_rem.rds"
ari_temp = "./data/temp_writeout_arikawa_2021_rem.rds"
nana_qd = "./data/quadrat_writeout_naname_2021.rds"
ari_qd = "./data/quadrat_writeout_arikawa_2021.rds"
height = "./data/seaweed_height.rds"
sal = "./data/conductivity_df.rds"

nana_seaweed = read_rds(nana_seaweed) |> mutate(location = "naname", station = as.double(station))
ari_seaweed = read_rds(ari_seaweed) |> mutate(location = "arikawa")
nana_qd = read_rds(nana_qd)
ari_qd = read_rds(ari_qd)
nana_temp = read_rds(nana_temp) |> select(-expID)
ari_temp = read_rds(ari_temp)

fish = read_rds(fish)
seaweed = bind_rows(ari_seaweed, nana_seaweed)
qd = bind_rows(nana_qd, ari_qd)
height = read_rds(height)
sal = read_rds(sal)
temp = bind_rows(ari_temp, nana_temp)

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model, data=HolzingerSwineford1939)

graph_sem(model = fit)

