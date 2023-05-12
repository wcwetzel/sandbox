# PERMANOVA for XLG
# May 2023

# This script has some code for doing a permutational MANOVA
# First it simulates data, and then it analyzes it with a PERMANOVA in vegan



# Load packages ####

library(vegan) # for multivariate functions
library(MASS) # just needed for simulating data
library(tidyverse)




# Simulate data ####

# data frame with plant IDs and trts; n = 60
d = data.frame(ID = 1:60, trt = rep(c('ctrl', 'trt'), each = 30))

# covariance matrix for chemistry values for ctrl and trt plants
SigmaCtrl = matrix(2, nrow=10, ncol=10)
diag(SigmaCtrl) = 4

SigmaTrt = matrix(4, nrow=10, ncol=10)
diag(SigmaTrt) = 10

# simulate chemistry data from multivariate normal distribution
mctrl = data.frame(exp(mvrnorm(n=30, mu = rep(c(0,2), each=5), Sigma=SigmaCtrl)))
mtrt = data.frame(exp(mvrnorm(n=30, mu = rep(c(2,0), each=5), Sigma=SigmaTrt)))
  # Note I'm simulating separately for ctrl and trt plants
  # I'm making ctrl plants low in cmpds 1-5 and high in cmpds 6-10
  # trt plants are high in cmpds 1-5 and low in cmpds 6-10

# put chemistry data for each treatment together
m = bind_rows(mctrl, mtrt) # m is just the chemical values

# add chemistry data to dataframe with plantIDs and trts
d = bind_cols(d, m) # d is a dataframe with chemical values and other data




# Examine the data with NMDS ####

# This is just for visualization. It's not part of the PERMANOVA
# If this were a real analysis, I would think about how to scale or transform
# the chemical data, but I'm not going to bother with that

# run the NMDS
ord = metaMDS(m, autotransform=FALSE)

# plot the NMDS
plot(ord)
ordihull(ord, groups = d$trt, col=c('blue', 'red'),
  display='sites')




# Run the PERMANOVA ####

out = adonis2(m ~ trt, data=d)
out




# Test for differences in multivariate dispersion between groups ####

# this can be treated as a test of beta diversity!

# first turn the chemical data matrix into a distance (difference) matrix
mdist = vegdist(m, method='bray')

# calculate the dispersions
beta = betadisper(mdist, group=d$trt)
beta

# test for differences
permutest(beta)
anova(beta)


