# *Part A*  Accept Reject for Beta(5, 5)
m <- 10000
k <- 0      # counter for accepted
j <- 0      # iterations
y <- numeric(m)

while (k<m){
      u <- runif(1)
      j <- j+1
      x <- runif(1) # Random Variate from g
      if ((210 * x^4 * (1-x)^4) > u){
          k <- k+1
          y[k] <- x
      }
}

# *Part B*  Plot density function
hist(y, prob=TRUE, col="mediumaquamarine", xlab="Random Variable", main="Beta(5,5)")
t <- seq(from=0, to=1, by=0.01)
dens <- (factorial(9)/(factorial(4)^2)) * t^4 * (1-t)^4
lines(t, dens, lty=1, col="maroon", lwd=2)

# *Part C*  Function of expected iterations
accepted <- function(m, n){
     b <- exp(lgamma(2*(n+1)))/(exp(lgamma(n+1))^2)
     number <- (0.5^4 * (1-0.5)^4)
     shame <- ceiling(b * number)
     m * shame
}

# *Part D*  Expected iterations for m=10,000 & n=4
accepted(10000, 4)



#  Checking stuff
cbind(mean(y), mean(rbeta(n, 5, 5)))
cbind(var(y), var(rbeta(n, 5, 5)))
