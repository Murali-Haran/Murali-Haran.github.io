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

Bmat <- matrix(c(1.6*10^20, 0, 0, 1.4*10^-20 ), 2,2)
solve(Bmat)
solve(solve(Bmat))


set.seed(1)
m=1000
A=matrix(rnorm(m*m),m,m)
B=matrix(rnorm(m*m),m,m)
system.time((AB=A%*%B))
system.time((AB2=crossprod(t(A),B)))

set.seed(2)
m=2000
A=matrix(rnorm(m*m),m,m)
B=matrix(rnorm(m*m),m,m)
system.time((AB=A%*%B))
system.time((AB2=crossprod(t(A),B)))

set.seed(2)
m=3000
A=matrix(rnorm(m*m),m,m)
B=matrix(rnorm(m*m),m,m)
system.time((AB=A%*%B))
system.time((AB2=crossprod(t(A),B)))


###### Memory allocation

LargeN <- 10000000
LargeN <- 10
myvec <- c(1)
for (i in 2:LargeN)
  {
    myvec <- c(myvec,myvec[i-1]+i)
  }


###### random number generation
set.seed(10)
rnorm(5)
## we want to save the state of the random number generator
oldseed = .Random.seed 
rnorm(5)

## Now do some other stuff
runif(28)
rnorm(18)

## restore the state of the random number generator from above
.Random.seed = oldseed
rnorm(5)


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
## Toy example to illustrate avoidance of inversion
########################################
## create a covariance matrix (pos def)
distmat <- diag( rep(0,3))
for (i in 1:3)
  for (j in 1:3)
    distmat[i,j] <- (i-j)^2

toycov <- exp(-1*distmat)

## find: toycov^-1 x toyvec
toyvec <- seq(1,3) # vector b

solve(toycov)
solve(toycov)%*%toyvec  ## intereested in A^-1 x b
##          [,1]
## [1,] 0.6723069
## [2,] 0.7558580
## [3,] 2.7096216

## find above without inversion
toychol <- chol(toycov) # find C s.t. C'C = Sigma   (note: C is upper triangular, U)
yvec <- backsolve(t(toychol), toyvec, upper.tri=FALSE) ## solve Ly=b
xvec <- backsolve(toychol, yvec) #
xvec
## [1] 0.6723069 0.7558580 2.7096216

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

######## toy image example
vecA=rep(1,25) # all white
vecB=c(rep(1,5),rep(0,15),rep(1,5)) # 5 white 20 black 5 white
vecC=c(rep(1,5), rep(0,3), rep(1,9),rep(0,3), rep(1,5))
toyImage=cbind(vecA, vecA, vecB, vecB, vecB, vecC, vecC, vecC, vecC, vecC, vecB, vecB, vecB, vecA, vecA)
dim(toyImage)
svdIm=svd(toyImage)
svdIm$d

########################
## large matrix example
randMat=matrix(rnorm(5000000), 5000, 1000)
svdMat=svd(randMat)

###############################
## condition number of a matrix
## E.g. from R.S.Wilson (Ciarlet, P.G. (1989), Introduction to
## Numerical Linear Algebra and Optimization)
########################

## create a covariance matrix (pos def)
distmat <- diag( rep(0,3))
for (i in 1:3)
  for (j in 1:3)
    distmat[i,j] <- (i-j)^2

toycov <- exp(-1*distmat)
C=toycov+0.01*diag(rep(1,3))
solve(toycov)
##            [,1]       [,2]       [,3]
## [1,]  1.1780952 -0.4920509  0.1594378
## [2,] -0.4920509  1.3620308 -0.4920509
## [3,]  0.1594378 -0.4920509  1.1780952
solve(C)
##            [,1]       [,2]       [,3]
## [1,]  1.1618062 -0.4790545  0.1534209
## [2,] -0.4790545  1.3390778 -0.4790545
## [3,]  0.1534209 -0.4790545  1.1618062


A=matrix(c(10,7,8,7,  7,5,6,5,  8,6,10,9,  7,5,9,10),4,4)
B=A + 0.01*diag(rep(1,4))

