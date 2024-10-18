## precision issues with matrix inversion
Rprof(NULL)
Amat <- matrix(c(1.6, 0, 0, 1.4), 2,2)
solve(solve(Amat))

Bmat <- matrix(c(1.6*10^308, 0, 0, 1.4*10^-308 ), 2,2)
solve(Bmat)
solve(solve(Bmat))
###
Error in solve.default(Amat) : 
  system is computationally singular: reciprocal condition number = 0
###

LargeN <- 10000000
LargeN <- 10
myvec <- c(1)
for (i in 2:LargeN)
  {
    myvec <- c(myvec,myvec[i-1]+i)
  }

myvec <- rep(NA, LargeN)
myvec[1] <- 1
for (i in 2:LargeN)
  {
    myvec[i] <- myvec[i-1]+i
  }

########################################
## Toy example to illustrate avoidance of inversion
########################################
## create a covariance matrix (pos def)
distmat <- diag( rep(0,3))
for (i in 1:3)
  for (j in 1:3)
    distmat[i,j] <- (i-j)^2

toycov <- exp(-1*distmat)

## find: toycov^-1 x toyvec
toyvec <- seq(1,3)

solve(toycov)
solve(toycov)%*%toyvec
         [,1]
[1,] 0.6723069
[2,] 0.7558580
[3,] 2.7096216

## find above without inversion
toychol <- chol(toycov)
> yvec <- backsolve(t(toychol), toyvec, upper.tri=FALSE)
> xvec <- backsolve(toychol, yvec)
> xvec
[1] 0.6723069 0.7558580 2.7096216

toyU <- chol(toycov)

Amat <- matrix(c(1e-8, 0, 0, 1e9), 2,2)
x <- c(2,7)
Amatinv <- matrix(c(1e8, 0, 0, 1e-9), 2,2) # we can do this, but the computer cannot!
## Amatinv%*%Amat works well (get identity matrix)
y <- Amatinv%*%x # not a problem
##      [,1]
## [1,] 2e+08
## [2,] 7e-09

## Now avoid matrix inversion
Achol <- chol(Amat)
## Achol%*%t(Achol) works well (get Amat)
## Now solve Ux = b where U is s.t. UU^t = A

####################################
## SVD, eigen decompositions
####################################
toysvd <- svd(toycov)
toysvd$u%*%diag(toysvd$d)%*%t(toysvd$v) # this works, returns toycov
toyeigen <- eigen(toycov)
toyeigen$vectors%*%diag(toyeigen$values)%*%t(toyeigen$vectors) # this works, returns toycov

> sqrt(svd(toycov%*%t(toycov))$d) ## sqrt of eigenvalues of M t(M) = eigenvalues of M
[1] 1.5294985 0.9816844 0.4888171
> toyeigen$values
[1] 1.5294985 0.9816844 0.4888171

########################################
# Hilbert matrix M for which M != solve(solve(M))
# (thanks to Jason Bernstein)
########################################

n <- 5 #10
M <- matrix(0,n,n)
for (i in 1:n)
  for (j in 1:n)
    M[i,j] <- 1/(i+j-1)

M.invinv <- solve(solve(M))
M - M.invinv

########################################
# Toy example for numerical integration
# Conditional pdf of mu
########################################

xConst <- 0.7
alphaConst <- 2
betaConst <- 2
  
muCondtl <- function(mu)
{
  val1 <- exp(-0.5*(xConst-mu)^2)/sqrt(2*pi)
  val2 <- (mu^(alphaConst-1)*(1-mu)^(betaConst-1))/beta(alphaConst,betaConst)

  return(val1*val2)
}

muvals <- seq(0,1,length=100)
plot(muvals, sapply(muvals, muCondtl))

muPrior <- function(mu)
  {
    val <- (mu^(alphaConst-1)*(1-mu)^(betaConst-1))/beta(alphaConst,betaConst)
    return(val)
  }
lines(muvals, sapply(muvals, muPrior), col="red")

########################################
## calculate normalizing constant
## for above function
########################################

## Using R's quadrature function
> integrate(muCondtl, 0, 1)
0.3818935 with absolute error < 4.2e-15
## So approximation to normalizing constant is 1/0.3818935

muCondtlNormalized <- function(mu)
  {
    return(muCondtl(mu)/0.3818935)
  }
## integrate(muCondtlNormalized,0.5,1)
## 0.5184336 with absolute error < 5.8e-15

########################################
## Riemann integration
########################################

approxList <- rep(NA,50)
for (i in 1:50)
  {
    n <- 10*i # number of intervals
    nodesRie <- seq(0,1,length=n)  ## divide up interval into n pieces
    muCondtlEval<- sapply(nodesRie, muCondtl)
    approxInt <- sum(muCondtlEval)/n
    approxList[i] <- approxInt
  }
plot(approxList, ylim=c(0.34,0.4))
abline(h=0.3818935)

########################################
## Simpson's rule NOT DONE
########################################
approxList <- rep(NA,50)
for (i in 1:50)
  {
    n <- 10*i # number of intervals
    nodesRie <- seq(0,1,length=n)  ## divide up interval into n pieces
    muCondtlEval<- sapply(nodesRie, muCondtl)
    approxInt <- sum(muCondtlEval)/n
    approxList[i] <- approxInt
  }
plot(approxList, ylim=c(0.34,0.4))
abline(h=0.3818935)

########################################
## Gaussian quadrature
########################################

########################################
## Laplace approximation
## for integral w.r.t. conditional posterior
########################################

#modeEst <- optimize(muCondtl, interval=c(0,1), maximum=TRUE)
modeEst <- optim(0.1, muCondtl, control=list(fnscale=-1), hessian=TRUE)
approxMean <- modeEst$par
approxVar <- -1*modeEst$hessian[1,1]

########################################
## overlay Laplace approx on top of
## actual pdf
########################################
xs <-seq(-0.1,1.1, length=100)
normpdf <- function(x)
  {
#    return(exp(-0.5*(x-approxMean)*(x-approxMean)/(approxVar/50)))
    return(exp(-0.5*(x-approxMean)*(x-approxMean)/approxVar))
  }
ys1 <- sapply(xs, muCondtl)
plot(xs,  ys1/max(ys1), col="blue", type="l")
ys2 <- sapply(xs, normpdf)
lines(xs, ys2/max(ys2), col="red")

