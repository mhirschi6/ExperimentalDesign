library(tidyverse)
library(gplots)
library(mosaic)

fish <- read_csv("FishData.csv")

fish %>% 
  ggplot(aes(x = Method, y = Score))+
  geom_boxplot()+
  geom_point()



plotmeans(Score~Method, data = fish)

fish %>% 
  ggplot(aes(sample = Score))+
  geom_qq()+
  geom_qq_line()

fish_model <- aov(Score~Method, data = fish)

plot(fish_model, which = 1:2)
favstats(Score~Method, data = fish)
summary(fish_model)


log_fish_model <- aov(log(Score)~Method, data = fish)
summary(log_fish_model)
