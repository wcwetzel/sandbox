# Trait-tolerance conceptual figure
# May 2026

# Load packages
library(tidyr)
library(ggplot2)

# Three separate functions
f1 = function(x) 0.164 * log(1 + 20 * (1-x))
f2 = function(x) 0.168 * log(1 + 5 * (1-x))
f3 = function(x) 0.288 * log(1 + 1 * (1-x))

f1 = function(x) 0.01 * (1-x^2)
f2 = function(x) 0.10 * (1-x^4)
f3 = function(x) 0.89 * (1-x^12)

f1 = function(x) 0.41 - 0.90 * x^2 + 0.49 * x^4
f2 = function(x) 0.39 - 0.05 * x^2 - 0.34 * x^4
f3 = function(x) 0.20 + 0.95 * x^2 - 1.15 * x^4

D = function(x) 1 + 44.44 * x^2 + 177.78 * x^4

f1 = function(x) (1 - x^4) / D(x)
f2 = function(x) ((44.44 * x^2) * (1 - x^4) ) / D(x)
f3 = function(x) ((177.78 * x^4) * (1 - x^4) ) / D(x)


D = function(x) 1 + 1975.31 * x^4 + 31604.94 * x^8

f1 = function(x) (1 - x^4) / D(x)
f2 = function(x) ((1975.31 * x^4) * (1 - x^4) ) / D(x)
f3 = function(x) ((31604.94 * x^8) * (1 - x^4) ) / D(x)

# Overall function
f = function(x) f1(x) + f2(x) + f3(x)

# Data frame
x = seq(0, 1, length=100)
dw = data.frame(
  d = x,
  f1 = f1(x),
  f2 = f2(x),
  f3 = f3(x),
  f = f(x)
)

# Reshape data
dl = pivot_longer(dw, cols = starts_with('f'),
                  names_to = 'f',
                  values_to = 'w')


# Plot
ggplot(data=dl, aes(x=d, y=w, color=f)) +
  geom_line()
