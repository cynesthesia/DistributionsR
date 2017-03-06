#  Problem 1: MLB2015 data Bootstrap and Jackknife
MLB <- read.csv(file.choose(), header=T)
attach(MLB)
R.game <- R / G


# (A) Estimate the correlation between R.game and AVG
cor(R.game, AVG)

# (B) Estimate the standard error of the sample correlation in (A).  Use Bootstrap with B=1000
B <- 1000
n <- length(AVG)
R <- rep(NA, length=B)

for(b in 1:B){
    i <- sample(1:n, size=n, replace=TRUE)
    R.game.b <- R.game[i]
    AVG.b <- AVG[i]
    R[b] <- cor(R.game.b, AVG.b)
}

se.R <- sd(R)
se.R

# (C) Answer (B) using the boot function with B=1000
library(boot)

r <- function(x, i){
    return(cor(x[i,1], x[i,2]))
}

mydata <- data.frame(R.game, AVG)
results <- boot(data=mydata, statistic=r, R=1000)
results
sd(results$t)

# (D) Estimate the correlation between R.game and OBP.
#     Use the function boot to estimate the bias and the standard error of the sample correlation.
#     Use B = 1000 replicates.
cor(R.game, OBP)
mydataD <- data.frame(R.game, OBP)
resultsD <- boot(data=mydataD, statistic=r, R=1000)
resultsD
sd(resultsD$t)

# (E) Answer (D) using Jackknife.
corr.hat <- cor(R.game, OBP)

n <- length(OBP)
corr.jack <- rep(NA, length=n)

for(i in 1:n){
   corr.jack[i] <- cor(R.game[-i], OBP[-i])
}
mean(corr.jack)

bias.est <- (n-1) * (mean(corr.jack) - corr.hat)
bias.est

se.est <- sqrt((n-1) * mean((corr.jack - mean(corr.jack))^2))
se.est


detach(MLB)
