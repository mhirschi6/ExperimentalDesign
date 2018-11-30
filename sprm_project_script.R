library(tidyverse)
library(mosaic)
library(car)
library(janitor)

dat <- read_csv("basketball2.csv")
dat <- clean_names(dat)
dat$person <- as.factor(dat$person)
dat$type <- as.factor(dat$type)
dat$x_u_feff_distance <- as.factor(dat$x_u_feff_distance)

dat %>%
  ggplot(aes(x = person, y = made))+
  geom_boxplot()

dat %>% 
  ggplot(aes(x = type, y = made))+
  geom_boxplot()

model <- aov(made ~ x_u_feff_distance + person + type + x_u_feff_distance:person,
             data = dat)

anova(model)

plot(model, which = 1:2)

favstats(dat$made ~ dat$x_u_feff_distance)
