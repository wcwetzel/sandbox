
library(vegan)
library(ggplot2)


# Fitness function
f = function(d, alpha, beta=-1) {
  beta * d ^ (1 / alpha) + 1
}

# Resistance values for each genotype
R = seq(0, 1, length=10)

# Damage values for each genotype
D = 1 - R

# Initial frequencies
fr_0 = rep(0.1, length=10)

# Number generations
g = 500

# alphas
alphas = c(0.5, 1, 2)

# diversities
s_mat = matrix(-1, nrow=g, ncol=length(alphas))

for(shape in 1:3) {

# alpha
alpha = alphas[shape]

# Frequency time series
fr = matrix(-1, nrow=g, ncol=10)
fr[1, ] = fr_0

# Shannon diversity time series
s = vector(mode='numeric')
s[1] = diversity(fr[1, ], index = 'shannon')

# Fitnesses of each genotype
w_i = exp(f(D, alpha=alpha))

# Simulation
for(i in 2:g) {

  # Use discrete time replicator equation to calc frequencies in next gen
  fr[i, ] = (fr[i-1, ] * w_i) / sum(fr[i-1, ] * w_i)

  # Calculate diversity
  s[i] = diversity(fr[i, ], index = 'shannon')
}

s_mat[, shape] = s

}

# Make data frame for plotting

dplot = data.frame(time = rep(1:g, length(alphas)),
    alpha = rep(c('insensitive', 'linear', 'sensitive'), each = g),
    diversity=c(s_mat[, 1], s_mat[, 2], s_mat[, 3])
)


# Plot


ggplot(data=dplot, aes(x=time, y=diversity, color=alpha)) +
  geom_line() +
  scale_color_manual(values = c('blue', 'black', 'red')) +
  xlab('Generations') +
  ylab('Genotypic diversity') +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 16),
    panel.grid = element_blank()
  )

