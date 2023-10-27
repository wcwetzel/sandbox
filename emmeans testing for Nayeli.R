# emmeans

library(emmeans)
library(lme4)
library(ggplot2)


# Simulate data
x = rep(c(0,1), each=20)
y = rpois(40, exp(0.5 + x*2))
re = rep(letters[1:10], each=4)
data = data.frame(x=as.factor(x),y=y,re=re)

# Run the poisson glmer
m = glmer(y ~ x + (1|re), data=data, family='poisson')

# Examine model
summary(m)

# Calculate means and confidence intervals
emmeans(m, specs = ~ x, level=0.95, adjust='none', type='response')

# Assemble means and confidence intervals into data.frame for plotting
data.plot = data.frame(x = c("0","1"),
                       means = c(1.55, 11.7),
                       lower = c(1.09, 10.29),
                       upper = c(2.2, 13.3))

# Plot means and confidence intervals
ggplot(data=data.plot, aes(x=x,y=means)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper))




