library(tidyverse)
library(gplots)
library(mosaic)

dat1 <- read_csv("Bonemineral.csv")
dat2 <- read_csv("Cancer.csv")

#Question number 2
dat1 %>% 
  ggplot(aes(x = Treatment, y = BMD))+
  geom_boxplot()+
  geom_point()



plotmeans(BMD~Treatment, data = dat1)

dat1 %>% 
  ggplot(aes(sample = BMD))+
  geom_qq()+
  geom_qq_line()

bmd_model <- aov(BMD~Treatment, data = dat1)

plot(bmd_model, which = 1:2)
favstats(BMD~Treatment, data = dat1)
summary(bmd_model)



#Question 3

dat2 %>% 
  ggplot(aes(x = type, y = days))+
  geom_boxplot()+
  geom_point()



plotmeans(days~type, data = dat2)

dat2 %>% 
  ggplot(aes(sample = days))+
  geom_qq()+
  geom_qq_line()

cancer_model <- aov(days~type, data = dat2)

plot(cancer_model, which = 1:2)
favstats(days~type, data = dat2)

summary(cancer_model)


#Question 4

dat2 %>% 
  ggplot(aes(x = type, y = days))+
  geom_boxplot()+
  geom_point()



plotmeans(days~type, data = dat2)

dat2 %>% 
  ggplot(aes(sample = days))+
  geom_qq()+
  geom_qq_line()

cancer_model <- aov(days~type, data = dat2)

plot(cancer_model, which = 1:2)
favstats(days~type, data = dat2)

summary(cancer_model)


log_cancer_model <- aov(log(days)~type, data = dat2)
summary(log_cancer_model)
