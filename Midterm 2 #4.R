#  Problem 4:  Recall that if U ~ Unif(0,1), then R = sigma*sqrt(-2*log(U)) ~ Rayleigh(sigma)
# (A) Use the function integrate to estimate E[R^3] where R ~ Rayleigh(5)
sigma <- 5

f <- function(x){
  return(x^3 * x/(sigma^2) * exp(-(x^2 / (2*sigma^2))))
}

integrate(f, lower=0, upper=Inf)

# Consistent with actual value
3 * sigma^3 * sqrt(pi/2)


# (B) Write a function that returns a MC estimate of E[R^3].  Use n=10000 iterations
MC <- function(n=10000){
  u <- runif(n)
  g <- (sigma * sqrt(-2*log(u)))
  return(mean(g^3))
}

MC(10000)


# (C) Histogram of the empirical distribution of the MC estimator
MC.ests <- replicate(5000, MC(10000))
  
hist(MC.ests, breaks=100, main="Distribution of simple MC estimator", prob=TRUE, xlim=range(MC.ests))
points(469.9928, 0, cex=2, pch=20, col="red")


# (D) Estimate E[R^3] using antithetic variables.  Use n=10000 iterations
MC.Anti <- function(n=10000){
  if (n%%2 == 1){
    stop("n has to be even!")
  }
  u <- runif(n/2)
  v <- c(u, 1-u)
  g <- (sigma * sqrt(-2 * log(v)))
  return(mean(g^3))
}
MC.Anti(10000)


# (E) Histogram of the empirical distribution of the estimator with antithetic variables
MC.Anti.ests <- replicate(5000, MC.Anti(10000))

hist(MC.Anti.ests, breaks=100, main="Distribution of MC estimator with antithetic", prob=TRUE, xlim=range(MC.ests))
points(469.9928, 0, cex=2, pch=20, col="red")


# (F) Empirical estimate of percent reduction in variance of antithetic compared to basic MC
(var(MC.ests)-var(MC.Anti.ests)) / var(MC.ests) * 100