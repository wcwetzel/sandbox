# Leaf removal round or ceiling calculations
# Apr 2024
# Exactly how does the number of leaves on a plant influence total damage
# when removing integer numbers of leaves

# Let's say 5% dam is the lowest non-zero damage
# Let's say 20 lvs is number of leaves on the smallest plant I use

# Number of leaves on each plant
n = 20:100

# How many lvs is 5%
n5 = n * 0.05

# Now round or ceiling that to get an integer number lvs to remove
n5round = round(n5)
n5ceil = ceiling(n5)

# Calculate actual total % dam using round or ceiling removed lvs
dam5round = n5round / n * 100
dam5ceil = n5ceil / n * 100

# Plot results
plot(n, dam5round, type='l', lty=1, ylim=c(0,100))
points(n, dam5ceil, type='l', lty=2)








