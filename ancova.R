library(tidyverse)
library(car)
library(mosaic)
library(janitor)

math <- read_csv("MathSelfEfficacy.csv") 
math <- clean_names(math)

hand <- read_csv("handwash.csv")
hand <- clean_names(hand)

math %>% 
  ggplot(aes(x = confidence_rating_mean, y = score))+
  geom_point()+
  facet_wrap(~gender)

math %>% 
  ggplot(aes(x = confidence_rating_mean, y = score, color = gender))+
  geom_point()+
  geom_smooth(method = "lm")

model1 <- lm(score ~ confidence_rating_mean + gender, contrasts = list(gender = contr.sum), data = math)
Anova(model1, type = 3)

plot(model1, which = 1:2)
leveneTest(math$score, math$gender, method = "median")
qqPlot(model1$residuals)

hand2 <- hand %>% 
  filter(cfu_before > cfu_after)

hand2 %>% 
  ggplot(aes(x = cfu_before, y = cfu_after, color = cleanser))+
  geom_point()+
  geom_smooth(method = "lm")

model2 <- lm(cfu_after ~ cfu_before + cleanser, data = hand2, 
             contrasts = list(cleanser = contr.sum))
Anova(model2, type = 3)
summary(model2)

qqPlot(model2$residuals)

hist(model2$residuals)
