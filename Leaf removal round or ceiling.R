# Leaf removal round or ceiling calculations
# Apr 2024
# Exactly how does the number of leaves on a plant influence total damage
# when removing integer numbers of leaves

library(ggplot2)

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
plot(n, dam5round, type='l', lty=1, ylim=c(0,30), las=1,
     xlab='Number of leaves plant',
     ylab='Percent damage')
points(n, dam5ceil, type='l', lty=2, col='red')
abline(h=5, lty=3, col='royalblue')



# What could the whole study look like?

# Randomize number of leaves >20
n = rpois(50000, 30)
n = n[n>=20]
hist(n)

# Treatments
trt = seq(0,1, by=0.05)

# Assign trts
trts = rep(trt, length=length(n))
trtspercent = trts*100

nprop = n*trts

nround = round(nprop)
nceil = ceiling(nprop)

# Calculate actual total % dam using round or ceiling removed lvs
damround = nround / n * 100
damceil = nceil / n * 100

# Plot results
plot(n, damround, type='p', pch=20,
     ylim=c(0, 100), las=1,
     xlab='Number of leaves plant',
     ylab='Percent damage')
points(n, damceil, type='p', lty=2, col='red')
abline(h=5, lty=3, col='royalblue')
abline(h=10, lty=3, col='royalblue')
abline(h=15, lty=3, col='royalblue')

lines(lowess(n, damround), col=1)
lines(lowess(n, damceil), col='red')

plot(damround ~ trtspercent, pch=1, cex=n/max(n)*1,
     ylim=c(0,100), xlim=c(0,100))
#points(damceil ~ trtspercent, col='red', cex=n/max(n)*1)
abline(a=0, b=1)

# Plot differences
rounddiff = damround - trtspercent
ceildiff = damceil - trtspercent

plot(rounddiff ~ n)

ggplot(data=data.frame(rounddiff=rounddiff, n=n, trtspercent=trtspercent),
       aes(x=n, y=rounddiff, color=trtspercent)) +
  geom_point() +
  xlab('Number of leaves') +
  ylab('Difference between assigned % and actual % dam') +
  labs(color='Assigned % dam')

plot(ceildiff ~ n)

plot(rounddiff ~ trtspercent, cex=n/max(n)*2)

ggplot(data=data.frame(rounddiff=rounddiff, n=n, trtspercent=trtspercent),
       aes(x=trtspercent, y=rounddiff, color=n)) +
  geom_point()

plot(damround ~ trts)
