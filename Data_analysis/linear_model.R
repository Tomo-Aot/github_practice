
library(tidyverse)
library(stats)

df = iris |> as_tibble()

model = lm(formula = Petal.Length ~ Petal.Width,
           data =df)

y = function(model, x){
  intercept = model$coefficient[1] # y切片
  coefficient = model$coefficients[2] # 係数
  return(x * coefficient + intercept)
}

y(model = model, x = 3)





