# Value n to n rows
# March 2023

# This script gives a toy example of converting from one row per plot to one row
# per insect. We often enter counts of insects per plot, but then we need to
# work with a dataframe with one row per insect. So we need to use the value of
# a column to add rows.

d = data.frame(
  plotID = 1:4,
  date = rep(1:2, each=2),
  daysToEmergance = rep(4:5, each=2),
  devRate = rep(log(1/4:5), each=2),
  trt = rep(letters[1:2], each=2),
  adults = c(3,4,1,2)
)
