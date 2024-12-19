# Modeling w(D)
# Dec 2024

library(ggplot2)

g = 1 # carbon acquisition per carbon in leaf area
p = c(0.1, 0.5, 0.9) # proportion of carbon stored in leaves (vs other organs)
C = 1 # current carbon in plant total
D = seq(0, 1, length=1000)

data = expand.grid(g=g, p=p, C=C, D=D)


data$dC.dt = with(data, (g*C*p*(1-D) - D*C*p))



ggplot(data, aes(x=D, y=dC.dt, group=p, color=p)) +
  geom_line()+
  scale_color_viridis_b()


