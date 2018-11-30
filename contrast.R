library(tidyverse)
library(car)
library(mosaic)
library(janitor)

iv <- read_csv("IVData.csv")
iv <- clean_names(iv)
iv$companies <- as.factor(iv$companies)

model1 <- lm(particles ~ companies, data = iv)
summary(model1)

contrasts(iv$companies) <- cbind(c(-0.5,1,-0.5),c(1,0,-1))
contrasts(iv$companies)

model2 <- lm(particles ~ companies, data = iv)
summary(model2)


nic <- read_csv("Nicotine.csv")
nic <- clean_names(nic)
nic$groups <- as.factor(nic$groups)

model3 <- lm(meconium ~ groups, data = nic)
summary(model3)

contrasts(nic$groups) <- cbind(c(0.5,0.5,-1),c(1,-1,0))
contrasts(nic$groups)
model4 <- lm(meconium ~ groups, data = nic)
summary(model4)
