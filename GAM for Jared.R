library(lme4)
library(mgcv)

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
