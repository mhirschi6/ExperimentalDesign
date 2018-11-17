library(tidyverse)
library(mosaic)
library(car)


ball <- read_csv("basketball1.csv")
ball$distance <- as.factor(ball$distance)
ball$block <- as.factor(ball$block)
model <- aov(shots ~ distance + block, data = ball)
anova(model)

favstats(shots ~ distance, data = ball)
favstats(shots ~ block, data = ball)

plot(model, which = 1:2)

ball %>%
  ggplot(aes(x = distance, y = shots))+
  geom_point()+
  geom_boxplot()

ball %>%
  ggplot(aes(x = block, y = shots))+
  geom_point()+
  geom_boxplot()
leveneTest(ball$shots, ball$distance)
