library(tidyverse)
library(car)
library(mosaic)
library(janitor)

pig <- read_csv("pigfeed.csv")
nozzle <- read_csv("NozzleDesign.csv")
nozzle$Nozzle <- as.factor(nozzle$Nozzle)
nozzle$Velocity <- as.factor(nozzle$Velocity)
battery <- read_csv("ActiveBattery.csv")

pig <- clean_names(pig)
pig$treatment <- as.factor(pig$treatment)
battery <- clean_names(battery)
battery$temperature <- as.factor(battery$temperature)
battery$electrolyte <- as.factor(battery$electrolyte)
battery$temp_chamber <- as.factor(battery$temp_chamber)

model <- aov(activated_lives ~ temperature + Error(electrolyte) + temp_chamber + 
               temperature:electrolyte, data = battery)
model1 <- aov(activated_lives ~ temperature + electrolyte + temp_chamber + 
                temperature:electrolyte, data = battery)

plot(model1, which = 1:2)

summary(model)


model2 <- aov(Response ~ Nozzle + Velocity, data = nozzle)
plot(model2, which = 1:2)

anova(model2)


model3 <- lm(final_weight ~ initial_weight + treatment , 
             contrasts = list(treatment = contr.sum), data = pig)
Anova(model3, type = 3)

interaction.plot(pig$treatment, pig$initial_weight, pig$final_weight)

plot(model3)



power.anova.test(groups = 5, between.var = var(c(0,0,0,0,10)), 
                 within.var = 11.67^2, sig.level = 0.05, n = 20)

power.anova.test(groups = 5, between.var = var(c(0,0,0,0,10)), 
                 within.var = 11.67^2, sig.level = 0.05, n = 20)
