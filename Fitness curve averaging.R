# Fitness curve averaging


x=seq(0,1,length=100)

yb = 2 * x^(1/5)
yd = -2 * x^(1/0.2)
yparmmean = 0 * x^(1/2.6)

ymean = yb + yd


plot(yb ~ x, type='l', col='green', ylim=c(-2,2))
points(yd ~ x, type='l', col='red')
points(ymean ~ x, type='l', col='blue', lty=2)
points(yparmmean ~ x, type='l', col='yellow', lty=2)



