# * Part A*  Joint density of three mixtures N(100, 169), N(125, 169), N(150, 169)
f.mix <- function(x, p1, p2){
  p1 <- 0.75
  p2 <- 0.20
  return(p1 * dnorm(x, mean=100, sd=13) + p2 * dnorm(x, mean=125, sd=13) + 
           (1-p1-p2) * dnorm(x, mean=150, sd=13))
}
f.joint <- function(x, y){
  return(f.mix(x) * f.mix(y))
}

#  Create sequences & Evaluate density
y <- x <- seq(from=75, to=175, length=50)
z <- outer(x, y, f.joint)

#  Plot in 3D
persp(x, y, z)

#  Customize plot & Save perspective matrix
M <- persp(x, y, z, theta=35, phi=20, expand=0.6, ltheta=300, shade=0.4,
           ticktype="detailed", xlab="X", ylab="Y", zlab="f(x,y)", col=c(rev(rainbow(666, start=0.58, end=0.95)), rainbow(666, start=0.58, end=0.95)),
           main="Joint Density with p1=0.75, p2=0.20, p3=0.05")


# *Part B*  Alternate approach with lattice and wireframe
install.packages("lattice")
library(lattice)

#  Create sequences
p1 <- 0.75
p2 <- 0.20
y <- x <- seq(from=75, to=175, length=50)

#  Create 50^2 x 2 matrix of all possible (x,y) pairs
xy <- expand.grid(x,y)

#  Evaluate joint density
z <- (p1 * dnorm(xy[, 1], mean=100, sd=13) + p2 * dnorm(xy[, 1], mean=125, sd=13) + 
        (1-p1-p2) * dnorm(xy[, 1], mean=150, sd=13)) *
  (p1 * dnorm(xy[, 2], mean=100, sd=13) + p2 * dnorm(xy[, 2], mean=125, sd=13) + 
     (1-p1-p2) * dnorm(xy[, 2], mean=150, sd=13)) 

#  Plot joint density in 3D
wireframe(z ~ xy[, 1] * xy[, 2], xlab="X", ylab="Y", main="Joint Density w/ p1=0.75, p2=0.20, p3=0.05",
          col="deepskyblue3")

# *Part C*  Create a contour plot
#  I ran the z in Part A again for Parts C & D
contour(z, main="Contour Plot of Joint Density")

# *Part D*  Contour Plot using Terrain Colours
filled.contour(z, color.palette = terrain.colors, main="Contour Plot of Joint Density")

# *Part E*  Generate 10,000 random pairs from the joint density
n <- 10000
x1 <- rnorm(n, mean=100, sd=13) 
x2 <- rnorm(n, mean=125, sd=13)
x3 <- rnorm(n, mean=150, sd=13)
k.x <- rmultinom(n, size=1, prob=c(0.75, 0.20, 0.05))
draws.x <- k.x[1,] * x1 + k.x[2,] * x2 + (1-k.x[1,]-k.x[2,]) * x3

y1 <- rnorm(n, mean=100, sd=13) 
y2 <- rnorm(n, mean=125, sd=13)
y3 <- rnorm(n, mean=150, sd=13)
k.y <- rmultinom(n, size=1, prob=c(0.75, 0.20, 0.05))
draws.y <- k.y[1,] * y1 + k.y[2,] * y2 + (1-k.y[1,]-k.y[2,]) * y3

#  Joint density (X and Y are independent)
draws.joint <- cbind(draws.x, draws.y)

# *Part F*  2D histogram using hist2d from gplots
install.packages("gplots")
library(gplots)
hist2d(draws.joint, nbins=50, col=rev(rainbow(50, start=0.35, end=0.965))
      , xlab="X", ylab="Y", main="2D Histogram of the Pairs")
