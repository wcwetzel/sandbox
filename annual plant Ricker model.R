# Annual plant Ricker model

N0 = 1
r0 = log(2)
d = 1/10000000*10
K = 1000000000*1/1

Nrange = seq(0,100, length=1000)
pgrs = r0 + d*Nrange

plot(Nrange, pgrs, type='l')


N = vector()
N[1] = N0
timesteps = 1000

for(i in 2:timesteps) {
  N[i] = N[i-1] * exp(r0 * (1 - d * N[i-1]))
}

time = 1:timesteps

plot(time, N, type='l')
