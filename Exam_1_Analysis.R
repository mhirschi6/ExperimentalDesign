library(tidyverse)
library(agricolae)
library(gplots)

#16
bat <- read_csv("BatteryLife.csv")
bat$Material <- as.factor(bat$Material)
bat_aov <- aov(FailureTime~Material, data = bat)

bat %>% 
  ggplot(aes(x = Material, y = FailureTime))+
  geom_boxplot()+
  geom_point()

bat %>% 
  ggplot(aes(sample = FailureTime))+
  geom_qq()+
  geom_qq_line()

plotmeans(FailureTime ~ Material, data = bat)

plot(bat_aov)

anova(bat_aov)
summary(bat_aov)



#17

fin <- read_csv("SurfaceFinish.csv")
fin$Feedrate <- as.factor(fin$Feedrate)
fin$DepthofCut <- as.factor(fin$DepthofCut)
fin_aov <- aov(SurfaceFinish~DepthofCut*Feedrate, data = fin)

fin %>% 
  ggplot(aes(x = Feedrate, y = SurfaceFinish))+
  geom_boxplot()+
  geom_point()
fin %>% 
  ggplot(aes(x = DepthofCut, y = SurfaceFinish))+
  geom_boxplot()+
  geom_point()

plotmeans(SurfaceFinish~Feedrate, data = fin)
plotmeans(SurfaceFinish~DepthofCut, data = fin)

interaction.plot(fin$Feedrate, fin$DepthofCut, fin$SurfaceFinish)
interaction.plot(fin$DepthofCut,fin$Feedrate, fin$SurfaceFinish)

plot(fin_aov, which = 1:2)

anova(fin_aov)
