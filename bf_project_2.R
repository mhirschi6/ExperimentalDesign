library(tidyverse)
library(mosaic)
library(agricolae)
library(car)
library(gplots)

dat <- read_csv("basketball.csv")
dat$basket_type <- as.factor(dat$basket_type)
dat$location_feet <- as.factor(dat$location_feet)
favstats(shots_made~location_feet+basket_type, data = dat)
dat %>% 
  ggplot(aes(x = basket_type, y = shots_made))+
  geom_boxplot()+
  geom_point()
dat %>% 
  ggplot(aes(x = location_feet, y = shots_made))+
  geom_boxplot()+
  geom_point()
my_aov <- aov(shots_made~basket_type*location_feet, data = dat)
anova(my_aov)

interaction.plot(dat$basket_type, dat$location_feet, dat$shots_made)
interaction.plot(dat$location_feet,dat$basket_type, dat$shots_made)
