# Problem 2  F(x): cdf of the standard Cauchy distribution (t-dist with df=1)

# (A): For x = 1, 2, ..., 10, compute a MC est of F(x) using Unif(0,x).
#     Compare the results with pcauchy function
#     Estimate the standard error and give 95% CIs

x <- seq(1, 10, length=10)
m <- 10000
cdf.U <- numeric(length(x))
sd.U <- numeric(length(x))

for (i in 1:length(x)){
  u <- runif(m, min=0, max=x[i])
  g <-  1 / (pi*(1 + u^2))
  cdf.U[i] <- mean(g) * x[i] + 0.5
  sd.U[i] <- x[i] * sqrt(mean((g - mean(g))^2) / m)
}

# Since the standard Cauchy distribution coincides with the t-distribution with df=1
alpha <- 0.05
CI.lower.U <- cdf.U - 1.96*sd.U
CI.upper.U <- cdf.U + 1.96*sd.U

phi <- pcauchy(x)

round(rbind(x, phi, cdf.U, sd.U, CI.lower.U, CI.upper.U), 5)
# phi: Theoretical value,  cdf.U: Empirical value,  sd.U: standard errors
# CI.lower.U & CI.lower.U: 95% confidence intervals using t-dist with df=1

# (B): Repeat (A) using the hit-or-miss method
x <- seq(1, 10, length=10)
m <- 10000
z <- rcauchy(m)
dim(x) <- length(x)
cdf.HM <- apply(x, MARGIN=1, FUN=function(x,z) {mean(z<x)}, z=z)
sd.HM <- apply(x, MARGIN=1, FUN=function(x,z) {sqrt(var(z<x)/m)}, z=z)

alpha <- 0.05
CI.lower.HM <- cdf.HM - 1.96*sd.HM
CI.upper.HM <- cdf.HM + 1.96*sd.HM

round(rbind(x, phi, cdf.HM, sd.HM, CI.lower.HM, CI.upper.HM), 5)


# (C): For each x, compute the empirical efficiency of the MC method (A) to the hit-or-miss method (B)
efficiency <- (sd.U/sd.HM)^2
efficiency