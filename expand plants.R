# Expanding plants to all time points: a toy example
# Nov 2026


# Load packages ####
library(tidyverse)


# Set up data ####
# Data parameters
nquad = 5 # number of quadrats
nplant = 5 # number of plants per quadrat
ntime = 3 # number of time points

# Set up data frame
d = data.frame(
  quadID = rep(1:nquad, each = nplant * ntime),
  plantID = rep(rep(1:nplant, times=ntime), times=nquad),
  time = rep(rep(1:ntime, each=nplant), times = nquad)
)


# Remove some rows, simulating dead or missing plants ####
dmissing = d |>
  # Drop plant 1 in quad 1 at time 3
  filter(!(quadID == 1 & plantID == 1 & time == 3)) |>
  # Drop plant 5 in quad 13 at times 2 and 3
  filter(!(quadID == 2 & plantID == 5 & time >1))


# Add a height col, which will be NA for missing plants ####
dmissing$height = runif(nrow(dmissing), 0, 1)


# Do the expansion ####
# I'm using nesting(quadID, plantID),
# but it would also work with group_by(quadID)
dfull = dmissing |>
  expand(nesting(quadID, plantID), time) |>
  left_join(dmissing, by = c('quadID', 'plantID', 'time'))

# Can use complete if you don't want to join manually after
dfull2 = dmissing |>
  complete(nesting(quadID, plantID), time)

