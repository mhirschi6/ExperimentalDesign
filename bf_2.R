library(tidyverse)
library(gplots)
library(mosaic)
library(agricolae)
library(janitor)
library(car)

dat <- read_csv("Cancer.csv")
dat$type <- as.factor(dat$type)
dat$gender <- as.factor(dat$gender)
dat1 <- read_csv("wear_no_rep.csv")
dat1$filler <- as.factor(dat1$filler)
dat1$proportion <- as.factor(dat1$proportion)

aov2 <- aov(dat1$wear~dat1$filler*dat1$proportion)
anova(aov2)
aov1 <- aov(dat1$wear~dat1$filler+dat1$proportion)
anova(aov1)

interaction.plot(dat1$filler, dat1$proportion, dat1$wear)

favstats(days~type+gender, data = dat)

model <- aov(days~type*gender, data = dat, contrasts = list(gender = contr.sum, type = contr.sum))
Anova(model,type = 3)



