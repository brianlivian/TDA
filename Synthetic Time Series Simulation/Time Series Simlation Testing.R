set.seed(5)
T=100
a1 = .5
x <- w <- rnorm(200, sd = 100)
for (t in 2:T) x[t] <- -a1*x[t-1] + w[t]
#layout(1:2)
plot(x, type="l")


x.ar <- ar(x, method = "mle")
x.ar$order
x.ar$ar
x.ar$ar + c(-1.96, 1.96)*sqrt(x.ar$asy.var)




require(graphics)
layout(1)
ts.plot(arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(10)))
# mildly long-tailed
ts.plot(arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5)))

# An ARIMA simulation
ts.sim <- arima.sim(list(order = c(1,1,2), ar = 0.7, ma = c(1, .23)), n = 200)
ts.plot(ts.sim)


require(graphics)
ts.sim <- arima.sim(list(order = c(1,0,0), ar = -0.7), n = 200)
par(mar = c(1, 1, 1, 1))
ts.plot(ts.sim)
ts.sim

