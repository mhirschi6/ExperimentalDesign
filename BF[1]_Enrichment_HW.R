library(tidyverse)
library(gplots)
library(mosaic)
library(agricolae)


dat <- read_csv("headinjury.csv")

dat %>% 
  ggplot(aes(x = Type, y = HeadInjury))+
  geom_boxplot()+
  geom_point()

plotmeans(HeadInjury~Type, data = dat)

dat %>% 
  ggplot(aes(sample = HeadInjury))+
  geom_qq()+
  geom_qq_line()

injury_model <- aov(HeadInjury~Type, data = dat)

plot(injury_model, which = 1:2)
favstats(HeadInjury~Type, data = dat)
summary(injury_model)


pairwise.t.test(dat$HeadInjury, dat$Type, "none")
pairwise.t.test(dat$HeadInjury,dat$Type, "bonferroni")

TukeyHSD(injury_model, "Type")
scheffe.test(injury_model,"Type", console = TRUE)



