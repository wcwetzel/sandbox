library(lme4)
library(mgcv)
library(brms)

x = runif(100, 0, 1)
ran = rep(letters[1:4], length=100)
y_hat = 1 + x^4
y_hat_ran = y_hat + c(0, 2, 4, 6)
y = rpois(100, exp(y_hat))


plot(y~x)
plot(y ~ as.factor(ran))

m = glmer(y ~ x + (1|ran), family='poisson')
summary(m)

m_gam = gam(y ~ s(x), family='poisson')
summary(m_gam)

plot(m_gam)

m2 = brm(y ~ s(x), data=data.frame(y=y, x=x), family='poisson')
plot(m2)
conditional_effects(m2)

m3 = brm(y ~ x, data=data.frame(y=y, x=x), family='poisson', iter=400)

