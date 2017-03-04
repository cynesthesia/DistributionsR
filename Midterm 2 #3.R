# Problem 3  Rayleigh(sigma) where sigma > 0

# (A) Simulate fake data from Rayleigh(sigma) [use sigma=5 and n=200]
#     Find the MLE of sigma numerically using the function mle and the function optim

# From midterm 1 #1, fake data was simulated from Rayleigh(sigma) two different ways.  Here, I'll use the Inverse transform method.
n <- 200
true.sigma <- 5
u <- runif(n)
Ray <- sqrt(-2 * true.sigma^2 * log(1-u))

mlogL <- function(theta, sx, slogR, n=200){
  sigma <- theta
  loglik <- -2*n*log(sigma) + slogR - (1/(2*sigma^2))*sx
  return(-loglik)
}

# The MLE of sigma using the function optim
optim(par=4.5, fn=mlogL, sx=sum(Ray^2), slogR=sum(log(Ray)), lower=4, upper=6, method="Brent")$par

# The MLE of sigma using the function MLE
MLE <- function(n, true.sigma){
  u <- runif(n)
  Ray <- sqrt(-2 * true.sigma^2 * log(u))
  return(optim(par=4.5, fn=mlogL, sx=sum(Ray^2), slogR=sum(log(Ray)), lower=4, upper=6, method="Brent")$par)
}
MLE(200, 5)

# Consistent with
sigmaMLE <- sqrt(sum(Ray^2)/(2*n))


# (B) Make a histogram of the empirical distribution of the MLE
#     Use the replicate and optim functions to simulate the MLE values (using m=5000 replications)
simulation <- replicate(5000, MLE(200, 5))

hist(simulation, breaks="scott", freq=FALSE, xlab="sigma", main="Distribution of MLE of sigma")
points(5, 0, cex=2, pch=20, col="red")
