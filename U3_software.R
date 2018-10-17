library(tidyverse)
library(gplots)
library(mosaic)
library(janitor)
library(agricolae)
library(RcmdrMisc)

#2A
soy <- read_csv("snapbean.csv")
soy <- clean_names(soy)
soy$sowdate <- as.factor(soy$sowdate)
soy$variety <- as.factor(soy$variety)

soy %>% 
  ggplot(aes(x = variety, y = Yield))+
  geom_boxplot()+
  geom_point()
soy %>% 
  ggplot(aes(x = sowdate, y = yield))+
  geom_boxplot()+
  geom_point()
plotmeans(yield~sowdate, data = soy)
plotmeans(yield~variety, data = soy)
interaction.plot(soy$sowdate, soy$variety, soy$yield)
interaction.plot(soy$variety, soy$sowdate, soy$yield)


#2B

my_aov <- aov(soy$yield~soy$sowdate*soy$variety)
plot(my_aov)

numSummary(soy$yield, groups=soy$sowdate)
numSummary(soy$yield, groups=soy$variety)

#2C
anova(my_aov)


#3
dat <- read_csv("programmers.csv")
dat <- clean_names(dat)

#3A
dat%>% 
  ggplot(aes(x = lg_sys_ex, y = time_pred_e))+
  geom_boxplot()+
  geom_point()
dat%>% 
  ggplot(aes(x = years_of_exp, y = time_pred_e))+
  geom_boxplot()+
  geom_point()
plotmeans(time_pred_e~lg_sys_ex, data = dat)
plotmeans(time_pred_e~years_of_exp, data = dat)
interaction.plot(dat$time_pred_e, dat$lg_sys_ex, dat$time_pred_e)
interaction.plot(dat$lg_sys_ex, dat$time_pred_e, dat$time_pred_e)

#3B
my_aov1 <- aov(dat$time_pred_e~dat$lg_sys_ex*dat$years_of_exp)
plot(my_aov1)
numSummary(dat$time_pred_e, groups=dat$lg_sys_ex)
numSummary(dat$time_pred_e, groups=dat$years_of_exp)

#3C
anova(my_aov1)


#4
y_aov1 <- aov(log(dat$time_pred_e)~dat$lg_sys_ex*dat$years_of_exp)
plot(my_aov1)
#numSummary(dat$time_pred_e["Logtime"], groups=dat$lg_sys_ex)
#numSummary(dat$time_pred_e("log"), groups=dat$years_of_exp)

anova(y_aov1)
