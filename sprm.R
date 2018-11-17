library(tidyverse)
library(mosaic)
library(car)
library(janitor)

hamster <- read_csv("HamsterData.csv")
hamster <- clean_names(hamster)


hamster %>% 
  ggplot(aes(x = day_length, y = response))+
  geom_boxplot()

hamster %>% 
  ggplot(aes(x = organ, y = response))+
  geom_boxplot()

#long way to calculate between block p-value
ham1 <- aov(response ~ day_length + hamster + organ + day_length:organ, 
           data = hamster)
anova(ham)

interaction.plot(hamster$day_length, hamster$organ, hamster$response)

#sum of squares make up the numbers for the between block. 
fday_length = 144/40
fday_length
#use the degrees of freedom. 
1-pf(fday_length, 1, 6)

#shortcut
ham <- aov(response ~ day_length + Error(hamster) + organ + day_length:organ, 
           data = hamster)
summary(ham)

plot(ham1, which = 1:2)
leveneTest(hamster$response,hamster$day_length)
favstats(response ~ day_length, data = hamster)

dog <- read_csv("Diabetic Dogs.csv")
dog$Dog <- as.factor(dog$Dog)
dog_model <- aov(LacticAcid ~ Method + Error(Dog) + Operations + Method:Operations,
                 data = dog)
summary(dog_model)

dog1 <- aov(LacticAcid ~ Method + Dog + Operations + Method:Operations,
            data = dog)
plot(dog1, which = 1:2)

