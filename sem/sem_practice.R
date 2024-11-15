# 2024-11-02
# structral equation modeling practice
# packages
library(tidyverse)
library(lavaan)
library(semPlot)
library(semTools)
library(tidySEM)

# modeling
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939)

graph_sem(model = fit)

