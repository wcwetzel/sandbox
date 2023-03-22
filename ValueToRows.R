# Value n to n rows
# March 2023

# This script gives a toy example of converting from one row per plot to one row
# per insect. We often enter counts of insects per plot, but then we need to
# work with a data frame with one row per insect. So we need to use the value of
# a column to add rows.
# The solution uses the uncount function in the tidyr package in tidyverse




# Load packages ####

library(tidyverse)




# Set up data frame ####

d = data.frame(
  plotID = rep(1:4, length=8),
  date = rep(1:4, each=2),
  daysToEmergance = rep(1:4 + 2, each=2),
  devRate = rep(log(1/(1:4 + 2)), each=2),
  trt = rep(letters[1:2], each=2, length=8),
  adults = c(3,4,1,2,1,2,2,5)
)




# Repeat rows per number of adults ####

dl = d %>%
  uncount(adults)




# Compare data frames ####

d
dl
# Works!
