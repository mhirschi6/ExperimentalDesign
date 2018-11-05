library(tidyverse)
library(mosaic)
library(car)

finger <- read_csv("FingerTapping.csv")
finger$Block <- as.factor(finger$Block)
finger$Drug <- as.factor(finger$Drug)


model <- lm(FingerTapping~Block + Drug, data = finger)

anova(model)
plot(model, which = 1:2)
favstats(FingerTapping~Block, data = finger)
favstats(FingerTapping~Drug, data = finger)
leveneTest(finger$FingerTapping, finger$Drug, center = "mean")

model1 <- lm(FingerTapping~Drug, data = finger)
anova(model1)
qqPlot(model$residuals)



auditor <- read_csv("auditor.csv")
auditor$block <- as.factor(auditor$block)
auditor$method <- as.factor(auditor$method)

model2 <- lm(score ~ method + block, data = auditor)
anova(model2)
leveneTest(auditor$score, auditor$method)

qqPlot(model2$residuals)
plot(model2, which = 1:2)
model3 <- lm(score ~ method, data = auditor)
anova(model3)
