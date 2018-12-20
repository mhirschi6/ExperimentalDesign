library(tidyverse)
library(mosaic)
library(car)
library(janitor)


soft <- read_csv("softdrinksyrup.csv")
soft <- clean_names(soft)
soft$pressure <- as.factor(soft$pressure)
soft$design <- as.factor(soft$design)
soft$speed <- as.factor(soft$speed)

model <- aov(frothing ~ pressure * design * speed, data = soft)
anova(model)

plot(model, which = 1:2)

soil <- read_csv("Soilrunoff.csv")
soil <- clean_names(soil)

model1 <- aov(y ~ rain * log * catch, data = soil)
anova(model1)
plot(model1)
