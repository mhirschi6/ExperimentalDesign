library(tidyverse)
library(gplots)
library(mosaic)

dat <- read_csv("IVData.csv")

ggplot(data = dat, aes(x = Companies, y = Particles))+
  geom_boxplot()+
  geom_point()
       

ggplot(data = dat, aes(x = Companies, y = Particles))+
  geom_point()

plotmeans(Particles~Companies, data = dat)

ggplot(data = dat, aes(sample = Particles))+
  geom_qq()+
  geom_qq_line()


favstats(Particles~Companies, data = dat)

model <- aov(Particles~Companies, data = dat)
plot(model, which = 1:2) # assumptions

summary(model)
