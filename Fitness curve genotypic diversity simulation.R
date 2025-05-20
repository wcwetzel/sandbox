# Fitness function curvature & genotypic diversity
# May 2024

# The parameter alpha controls curvature

# Load packages
library(vegan)
library(ggplot2)


# Fitness function:
  # d = damage, alpha = curvature, beta = -1 makes fitness decline
f = function(d, alpha, beta=-1) {
  exp(beta * d ^ (1 / alpha) + 1)
}

# alphas: 3 values for simulations
alphas = c(0.5, 1, 2)


# plot fitness functions
curve(f(x, alpha=alphas[1]), xlab='Damage', ylab='Absolute fitness',
      col='blue', las=1)
curve(f(x, alpha=alphas[2]), add=TRUE,
      col='black')
curve(f(x, alpha=alphas[3]), add=TRUE,
      col='red')


# Resistance values for each genotype
R = seq(0, 1, length=10)


# Damage values for each genotype
D = 1 - R


# Initial frequencies
fr_0 = rep(0.1, length=10)


# Number generations
g = 200


# diversities
s_mat = matrix(-1, nrow=g, ncol=length(alphas))


# Loop over the 3 alpha values
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
  w_i = f(D, alpha=alpha)


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

