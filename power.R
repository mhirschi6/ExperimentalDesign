power.anova.test(groups = 5, between.var = var(c(0,0,0,0,10)), 
                 within.var = 11.67^2, sig.level = 0.05, n = 25)

power.anova.test(groups = 5, between.var = var(c(0,0,0,0,10)), 
                 within.var = 11.67^2, sig.level = 0.05, power = 0.869)

power.anova.test(groups = 4, between.var = var(c(0,0,0,5)), 
                 within.var = 11.67^2, sig.level = 0.05, power = 0.85)

power.anova.test(groups = 4, between.var = var(c(0,0,0,5)), 
                 within.var = 11.67^2, sig.level = 0.05, n = 20)
