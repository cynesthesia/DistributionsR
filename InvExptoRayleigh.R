# *Part A*  Get X ~ Exp from Unif(0,1), then transform X to get Rayleigh
n <- 10000
lambda <- 4
sigma <- 1
u <- runif(n)
x <- -log(1-u) / lambda  
r1 <- rep(NA, length=n)
for(i in 1:n){
    r1[i] <- sqrt(2*sigma^2*lambda*x[i])
}

# *Part B*  Inverse Transform Method
n <- 10000
sigma <- 1
u <- runif(n)
r2 <- sqrt(-2 * sigma^2 * log(1-u))
  
# *Part C*  Compare sample quantiles using a QQplot
qqplot(r1, r2, xlab="Transforming Exp(lambda)", ylab="Inverse Transform", 
       main="Rayleigh(1) Sample Quantiles")
