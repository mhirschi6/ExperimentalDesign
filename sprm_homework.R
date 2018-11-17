library(tidyverse)
library(car)
library(mosaic)


aux <- read_csv("auxin.csv")

model <- aov(days ~ auxin + Error(plant) + deblading + auxin:deblading, data = aux)
summary(model)


