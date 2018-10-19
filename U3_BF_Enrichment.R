library(tidyverse)
library(gplots)
library(agricolae)
library(janitor)
library(car)

cancer <- read_csv("Cancer.csv")
cancer$type <- as.factor(cancer$type)
cancer$gender <- as.factor(cancer$gender)

singer <- read_csv("singerheights.csv")
singer$gender <- as.factor(singer$gender)
singer$part <- as.factor(singer$part)

cancer_anova <- aov(age~type*gender, data = cancer)
anova(cancer_anova)

cancer_anova1 <- aov(age~type*gender, data = cancer, contrasts = list(type = contr.sum, gender = contr.sum))
Anova(cancer_anova, type = 3)

singer_anova <- aov(height~gender*part, data = singer)
anova(singer_anova)

singer_anova1 <- aov(height~part*gender, data = singer, contrasts = list(part = contr.sum, gender = contr.sum))
Anova(singer_anova1, type = 3)
