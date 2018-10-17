echo=FALSE
include=FALSE
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)
library(tidyverse)
library(gplots)
library(mosaic)
library(agricolae)


dat <- read_csv("Dryer.csv")

dat %>% 
  ggplot(aes(x = Temperature, y = Time))+
  geom_boxplot()+
  geom_point()

plotmeans(Time~Temperature, data = dat)

dat %>% 
  ggplot(aes(sample = Time))+
  geom_qq()+
  geom_qq_line()

dryer_model <- aov(Time~Temperature, data = dat)

plot(dryer_model, which = 1:2)

favstats(Time~Temperature, data = dat)
summary(dryer_model)
