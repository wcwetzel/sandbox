# Simulating data for Jared
# Mar 2026

library(ggplot2)
library(lme4)
library(brms)

# 1 transect
# 5 elevations
# 3 sun and 3 shade plots per elevation

# 3 treatments: control, tip, hole
# 5 plants per treatment per plot

# 5 plants  * 3 treatments = 15 plants per plot
# 15 plants per plot * 6 plots per elevation = 90 plants per elev
# 90 plants per elev * 5 elevations = 450 plants total

# 6 plots per elev * 5 elevs = 30 plots

# Set up data.frame ####
d = data.frame(plantID = 1:450)
d$elevation.m = rep(c(1500, 1875, 2250, 2625, 3000), each = 90)
d$elev.std = as.numeric(scale(d$elevation.m))
d$plotID = rep(1:30, each = 15)
d$shade.sun = rep(c('shade', 'sun'), each=15)
d$shade.sun.num = rep(c(0,1), each = 15)
d$damage.trt = rep(c('ctrl', 'tip', 'holes'), times=150)
d$damage.trt.num = rep(c(0,0,1), times=150)

# Parameters ####
mu.fruits = 12
beta.elev = -2.5
beta.sun = 1
beta.dam = -5
beta.elev.dam = -2.5

# Fixed effects ####
d$mu.pred = mu.fruits +
  d$elev.std * beta.elev +
  d$shade.sun.num * beta.sun +
  d$damage.trt.num * beta.dam +
  d$elev.std * d$damage.trt.num * beta.elev.dam

d$mu.pred = pmax(d$mu.pred, 0)

ggplot(d, aes(x=elevation.m, y=mu.pred, color=damage.trt)) +
  geom_point() +
  facet_grid(~shade.sun) +
  geom_smooth()


# Plot random effects ####
d$plot.RE = (rep(rnorm(30, sd=0.1), each=15))


d$mu.RE = d$mu.pred + d$plot.RE
d$mu.RE = pmax(d$mu.RE, 0)


ggplot(d, aes(x=elevation.m, y=mu.RE, color=damage.trt)) +
  geom_point() +
  facet_grid(~shade.sun) +
  geom_smooth(method=lm) #+
  #coord_cartesian(ylim=c(0,11))



# Simulation of fruit counts ####

d$fruits = rnbinom(nrow(d), mu=d$mu.RE, size=5)
#d$fruits = rpois(nrow(d), d$mu.RE)


ggplot(d, aes(x=elevation.m, y=fruits, color=damage.trt)) +
  geom_point() +
  facet_grid(~shade.sun) +
  geom_smooth(method=lm)



# Model ####

m = brm(fruits ~ elev.std * damage.trt + shade.sun + (1|plotID), data=d,
        family=negbinomial(),
        backend = "cmdstanr",
        iter=1000,
        chains=6,
        cores=6)


# export csv for Jared
d.out = data.frame(plantID = d$plantID,
                   plotID = d$plotID,
                   elevation.m = d$elevation.m,
                   shade.sun = d$shade.sun,
                   damage.trt = d$damage.trt,
                   fruits = d$fruits)

write.csv(d.out, 'Jared_simulated.csv')
