# Modeling w(D)
# Dec 2024

library(ggplot2)

g = 1 # carbon acquisition per carbon in leaf area
p = c(0.1, 0.5, 0.9) # proportion of carbon stored in leaves (vs other organs)
C = c(0.1, 1, 10) # current carbon in plant total
D = seq(0, 1, length=1000)

data = expand.grid(g=g, p=p, C=C, D=D)


data$dC.dt = with(data, (g*C*p*(1-D) - D*C*p))



ggplot(data, aes(x=D, y=dC.dt, group=p, color=p)) +
  geom_line()+
  facet_grid(~C) +
  scale_color_viridis_b()


# Maybe need to think about how leaf area relates to carbon acquisition?

# Maybe efficiency of C capture per C leaf area declines with increasing Cp
# g = a + e*C*p, where a = max acquisition and e = efficiency

a = 1
e = -0.1

data = expand.grid(a=a, e=e, p=p, C=C, D=D)

data$dC.dt = with(data, ((a + e*C*p)*C*p*(1-D) - D*C*p))

ggplot(data, aes(x=D, y=dC.dt, group=p, color=p)) +
  geom_line()+
  facet_grid(~C) +
  scale_color_viridis_b()


data$dC.dt.perC = data$dC.dt / data$C

ggplot(data, aes(x=D, y=dC.dt.perC, group=p, color=p)) +
  geom_line()+
  facet_grid(~C, scale='free_y') +
  scale_color_viridis_b()


