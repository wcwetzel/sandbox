# Learning about the multivariate normal

# Started this to help Julia become familiar with the mvnorm distribution


# Load packages ####
library(MASS)

?mvrnorm


# Set parameter values
m = 1 # mean
s = 1 # sd


# Simulate from normal distribution ####
# with mean = m, sd = s
# to get a feel for normal distribution

rnorm(4, mean = m, sd = s)


# Simulate from multivariate normal distribution ####
# with mean m, variance = s^2, and specified covariance values
# https://en.wikipedia.org/wiki/Covariance_matrix
# to get a feel for the mvnorm distribution

mus = c(m, m, m, m) # means of a vector of 4 values (all have same means m)
Sigma = matrix(0, nrow=4, ncol=4) # Build covariance matrix
diag(Sigma) = s^2 # the diagonal of the covariance matrix holds the variances
Sigma[1,2] = Sigma[2,1] = s^2 # covariance between point 1 & 2 = variance
Sigma[3,4] = Sigma[4,3] = s^2 / 2 # covariance between point 1 & 2 = variance/2

Sigma # take a look at covariance matrix

mvrnorm(n=1, mu = mus, Sigma = Sigma)

# Notice that values 1 and 2 are very STRONGLY correlated.
# Values 3 and 4 and correlated, but less strongly.
# Values 1,2 and values 3,4 vary independently.
# This is all from how we set up the covariance matrix.
# We can use statistics (Bayes or max likelihood) to estimate the parameters
# of the covariance matrix with real data. (here we're simulating fake data)