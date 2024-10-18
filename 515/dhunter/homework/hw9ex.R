## Here is some code in R that uses a numerical root-finding algorithm to find
## the unique solution in (0,1) to the equation x^3 = U, where U is taken to 
## be a uniform (0,1) variable.

# Set up F and its numerical inverse function:
F <- function(x) x^3
FminusU <- function(x, u) F(x) - u
Finv <- function(u) uniroot(FminusU, interval=c(0,1), u=u)$root

# Now generate a lot of uniforms and apply Finv to them:
u <- runif(1e5)
system.time (x <- sapply(u, Finv))  # use system.time to see how long it takes

# Verify that the sample appears to come from the correct density:
hist(x, freq=FALSE)
xx <- seq(0, 1, len=200)
lines(xx, 3 * xx^2, col=2)



## Here is some code in R that uses the eigenvalue decomposition
## to generate multivariate normal vectors with a particular mean mu and
## variance matrix Sigma:

## Define the mu vector and Sigma matrix:
mu <- c(1, -2)
Sigma <- matrix(c(16, -15, 15, 25), 2, 2)

# Now use the eigen decomposition to find X such that X %*% t(X) equals Sigma:
e <- eigen (Sigma, symmetric=TRUE) # The algorithm is slightly more efficient
                                   # if symmetric=TRUE is given.
X <- e$vec %*% (t(e$vec) * sqrt(e$val)) 

# Verify that it worked:
X %*% t(X)

# Now multiply X by a lot of columns of standard normals and add mu:
Y <- X %*% matrix(rnorm(2e4), nrow=2) + mu

# Here is a plot of the result:
plot(t(Y))

