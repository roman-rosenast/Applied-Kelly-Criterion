drift <- function(r,t) {
  return(0.1*r)
}

volatility <- function(r,t) {
  return(0.05*r)
}

euler.maruyama <- function(t0, t1, h, r0, a, b) {
  t <- seq(t0, t1, by = h)
  z <- rnorm(length(t))
  r <- rep(r0,length(t))
  for (i in 2:length(t)) {
    r[i] <- r[i-1] + a(r[i-1],t[i-1])*h + b(r[i-1],t[i-1])*z[i-1]*sqrt(h)
  }
  return(r)
}

r <- euler.maruyama(0, 3, 1, 0, drift, volatility)
t <- seq(0, 10^1)
f1 <- rep(1, 10^1)
f2 <- rep(1, 10^1)
fopt <- rep(1, 10^1)
for (i in 2:length(t)) {
  f1[i] <- max(f1[i-1] + f1[i-1]*0.5*(r[i]-r[i-1]), 0)
  f2[i] <- max(f2[i-1] + f2[i-1]*60*(r[i]-r[i-1]), 0)
  fopt[i] <- max(fopt[i-1] + fopt[i-1]*40*(r[i]-r[i-1]), 0)
}
data <- data.frame(
  lower = f1,
  higher = f2,
  optimal = fopt,
  time = t
)
ggplot(data, aes(time)) + 
  geom_line(aes(y=lower, colour="lower")) +
  geom_line(aes(y=higher, colour="higher")) +
  geom_line(aes(y=optimal, colour="optimal"))

finalWealth <- function(f) {
  w <- 1
  temp <- 0
  u <- runif(10^2, -1, 1.1)
  for (i in 2:10^2) {
    gain <- w*f*u[i]
    temp <- max(w + gain, 0)
    w <- temp
  }
  return(w)
}

low <- replicate(10^4, finalWealth(0.05))
high <- replicate(10^4, finalWealth(0.2))
opt <- replicate(10^4, finalWealth(0.1360544218))

mean(low)
sd(low)
mean(high)
sd(high)
mean(opt)
sd(opt)
