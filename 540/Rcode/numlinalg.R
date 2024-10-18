############################################################
## Illustrate cost of multivariate normal generation
## as a function of the matrix size
############################################################
library(MASS)

N = 2000 # or 10, 50, 100, 1000
randmean = rnorm(N)
randmat = matrix(rnorm(N*N), N,N)
covmat = randmat%*%t(randmat)

system.time(foo = mvrnorm(n=1, mu=randmean, Sigma=covmat))

############################################################
##### Illustrate numerical instability of matrix inversion
############################################################
A = matrix(c(1.6,0,0,1.4/10), 2,2)
Ainv = solve(A)
Ainv%*%A

B = matrix(c(1.6*10^7,0,0,1.4*10^(-7)), 2,2)
Binv = solve(B)
Binv%*%B

B = matrix(c(1.6*10^8,0,0,1.4*10^(-8)), 2,2)
Binv = solve(B)
Binv%*%B

B = matrix(c(1.6*10^308,0,0,1.4*10^(-308)), 2,2)
Binv = solve(B)
Binv%*%B


#########################
##### 
#########################

## Illustrate the importance of memory allocation
## versus dynamic memory allocation (dynamic=allocate
## new memory as needed rather than all in advance)

N = 10000000 #1000000
M = 50 # 500
A = matrix(rnorm(N*M), N, M)

## compute row sums by looping over the matrix rows
## memory for the vector to hold row sums (rowsumvec)
## is being allocated dynamically
computeRowSum1 = function(mat)
  {
    rowsumvec = c() # initialize vector
    for (i in 1:N)
      rowsumvec = c(rowsumvec, sum(mat[i,])) # new row sum goes into vector
    return(rowsumvec)
  }
system.time(myrowsum = computeRowSum1(A))

## now memory is allocated ahead of time. Much faster!
## is being allocated dynamically
computeRowSum2 = function(mat)
  {
    rowsumvec = rep(NA, N) # create a vector of length N (number of rows)
    for (i in 1:N)
      rowsumvec[i] = sum(mat[i,])
    return(rowsumvec)
  }

system.time(myrowsum2 = computeRowSum2(A))
system.time(myrowsum3 = apply(A,1, sum))

##################################
### another example of using apply
##################################
N = 1000000
myxs = rnorm(N)
thresh = 0.3
bar = function(xs)
{
  for (i in 1:N)
    if (xs[i]>thresh)
      xs[i] = thresh
  return(xs)
}

quux = function(xs,thresh)
{
  threshfun = function(x)
    {
      if (x>thresh)
        return(thresh)
      else
        return(x)
    }
  return(sapply(xs, threshfun))
}

system.time(xsnew = bar(myxs))
system.time(xsnew2 = quux(myxs,0.3))

#########################
## example for profiling R code
#########################

## function that generates a multivariate normal
## with covariance matrix = M*M^t where M is a matrix of N normal(0,1)
library(MASS)
generateMN = function(N)
  {
    randmean = rnorm(N)
    randmat = matrix(rnorm(N*N), N,N)
    covmat = randmat%*%t(randmat)
    X = mvrnorm(n=1, mu=randmean, Sigma=covmat)
    return(X)
  }
Rprof("profile1.out") # start profiling
foo = generateMN(1000)
Rprof(NULL) # stop profiling
##system.time(foo = mvrnorm(n=1, mu=randmean, Sigma=covmat))
summaryRprof(filename="Rprof.out")


############################
### Eigendecompositions

## M=1000 ## replications for simulation studies
N=4 #20
#### create locations, not equally spaced
set.seed(1)
SIMLOC=sort(runif(N)) ## generate locns, then sort them in order (for plotting ease later)
DISTMAT=as.matrix(dist(SIMLOC))
SIMLOCMAT=cbind(SIMLOC, rep(0,N)) # each vector in this matrix is a locn (in 2D w/ 2nd dimension 0)
BETA=5
PHI=0.6
SIGMASQ=1

X=SIMLOC
P=X%*%solve(t(X)%*%X)%*%t(X)
Pperp=diag(length(X))-P
L=eigen(P) ## p (1) non-zero eigenvalues
K=eigen(Pperp) # n-p (n-1) non-zero eigenvalues

##PHI=0.6
##SIGMASQ=1
### regular eigen-decomposition, just for fun
SIGMA=SIGMASQ*exp(-DISTMAT/PHI)
eigenSig=eigen(SIGMA)
foo=eigenSig$vectors%*%diag(eigenSig$values)%*%t(eigenSig$vectors)
### now consider how the spatial random effects may be rewritten
SIGMASTARINV=t(L$vec)%*%solve(SIGMA)%*%t(L$vec)
baz=rnorm(N)
W=chol(SIGMA)%*%baz
P%*%W + Pperp%*%W ## this is obviously the same as W above

## for symmetric matrix, SVD does the same thing 
svdSig=svd(SIGMA)

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


