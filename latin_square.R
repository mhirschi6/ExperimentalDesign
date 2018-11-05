library(tidyverse)
library(mosaic)
library(car)

boxplot(mpg~blend, data = mpg)
milk <- read_csv("MilkYield.csv")
mpg <- read_csv("mpg.csv")
#mpg$model <- as.factor(mpg$model)
#mpg$blend <- as.factor(mpg$blend)
mpg$driver <- as.factor(mpg$driver)
mpg_aov <- lm(mpg ~ driver + model + blend, data = mpg)
anova(mpg_aov)

qqPlot(mpg_aov$residuals)

leveneTest(mpg$mpg ~ mpg$blend)
