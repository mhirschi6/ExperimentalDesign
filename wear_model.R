library(tidyverse)
library(mosaic)
library(gplots)
library(agricolae)
library(janitor)

dat <- read_csv("wear.csv")
dat2 <- read_csv("Pig out.csv")
dat3 <- clean_names(dat2)
dat$prop <- as.factor(dat$prop)
dat$filler <- as.factor(dat$filler)


dat %>% 
  ggplot(aes(x = prop, y = wear1))+
  geom_boxplot()+
  geom_point()
dat %>% 
  ggplot(aes(x = filler, y = wear1))+
  geom_boxplot()+
  geom_point()

interaction.plot(dat$prop, dat$filler, dat$wear1)

favstats(dat$wear1~dat$prop+dat$filler)

my_aov <- aov(log(dat$wear1) ~ dat$filler * dat$prop)
anova(my_aov)

plot(my_aov)

