library(tidyverse)
library(mosaic)
library(car)
library(janitor)


quality <- read_csv("Qualitydata.csv")
quality <- clean_names(quality)

quality %>% 
  ggplot(aes(x = fee_level, y = quality))+
  geom_point()+
  geom_boxplot()

quality %>% 
  ggplot(aes(x = scope, y = quality))+
  geom_point()+
  geom_boxplot()

quality %>% 
  ggplot(aes(x = supervision, y = quality))+
  geom_point()+
  geom_boxplot()

interaction.plot(quality$fee_level, quality$scope, quality$quality)
interaction.plot(quality$supervision, quality$scope, quality$quality)
interaction.plot(quality$supervision, quality$fee_level, quality$quality)


model <- lm(quality ~ fee_level * scope * supervision, data = quality)
anova(model)

plot(model, which = 1:2)


qqPlot(model)
