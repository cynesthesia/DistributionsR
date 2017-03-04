#  Problem 5  (6.1 from textbook)
#  Estimate the MSE of the level k trimmed means for random samples of size 20 generated from a standard Cauchy distribution.
#  (The target parameter theta is the center or median; the expected value does not exist)
#  Summarize the estimates of MSE in a table for k = 1, 2, ..., 9
n <- 20
m <- 1000
K <- n/2 - 1
mse <- numeric(n/2)

trimmed.mse <- function(n, m, k){
  tmed <- numeric(m)
  for (i in 1:m){
    x <- sort(rcauchy(n, location=0, scale=1))
    tmed[i] <- median(x[(k+1):(n-k)])
  }
  mse.est <- mean(tmed^2)
}

for (k in 0:K){
  mse[k+1] <- trimmed.mse(n=n, m=m, k=k)
}
mse
