# Kelsen's alfalfa damage x drought experimnt
# Apr 2026


# Load packages ####
library(emmeans)



# Load data ####
d = read.csv('Alfalfa Damage and Drought Treatments - Final Data.csv')

# Run model ####

m = lm(final_growth_cm ~ damage_trt * water_trt, data=d)
summary(m)
aov(m)
anova(m)

m.noint = lm(final_growth_cm ~ damage_trt + water_trt, data=d)
m.dam = lm(final_growth_cm ~ damage_trt, data=d)
m.water = lm(final_growth_cm ~ water_trt, data=d)

anova(m, m.noint)
anova(m.noint, m.dam)
anova(m.noint, m.water)


emmeans(m, ".")
emm = emmeans(m, ~ water_trt * damage_trt)

pairs(emm, adjust = "fdr")

summary(aov(final_growth_cm ~ damage_trt * water_trt, data=d))


