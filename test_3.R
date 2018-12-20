library(tidyverse)
library(mosaic)
library(car)


tools <- read_csv("toollife.csv")
tools$Cutting <- as.factor(tools$Cutting)
tools$Tool <- as.factor(tools$Tool)
tools$Angle <- as.factor(tools$Angle)

model <- aov(toollife ~ Cutting * Tool * Angle, data = tools)
plot(model, which = 1:2)

anova(model)



corrosion <- read_csv("CorrosionData.csv")
corrosion$Temperature <- as.factor(corrosion$Temperature)
corrosion$Coating <- as.factor(corrosion$Coating)
interaction.plot(corrosion$Temperature, corrosion$Coating, corrosion$Resistance)

corrosion %>% 
  ggplot(aes(x = Temperature, y = Resistance))+
  geom_point()+
  geom_boxplot()
corrosion %>% 
  ggplot(aes(x = Coating, y = Resistance))+
  geom_point()+
  geom_boxplot()

model <- aov(Resistance ~ Temperature * Coating, data = corrosion)

plot(model, which = 1:2)
qqPlot(model)

anova(model)
