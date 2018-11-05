library(tidyverse)
library(mosaic)
library(car)


dat <- read_csv("marketing.csv")
dat$day <- as.factor(dat$day)
dat$shelfht <- as.factor(dat$shelfht)

dat_aov <- lm(Sales ~ shelfht + day, data = dat)
anova(dat_aov)
leveneTest(dat$Sales, dat$shelfht)
qqPlot(dat_aov$residuals)

dat_aov1 <- lm(Sales~shelfht, data = dat)
anova(dat_aov1)
