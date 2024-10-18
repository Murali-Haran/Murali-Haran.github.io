############################
## Probability Example 3
## Simulate from Gaussian
## process, w/ mean 0
## covariance between X_i and X_j=sigmasq*exp(-d_ij/phi)
## where d_ij is Euclidean distance between X_i and X_j
## sigmasq, phi> 0 are parameters of model
## e.g. sigmasq=1, phi =0.8
## simulate locations for random variables according to
## Unif(0,1)
############################
library(MASS)
N=30
phi=0.8
sigmasq=1
beta=2
set.seed(1)
loc=sort(runif(N)) ## generate locns, then sort them in order (for plotting ease later)
locmat=cbind(loc, rep(0,N)) # each vector in this matrix is a locn (in 2D w/ 2nd dimension 0)
distmat=as.matrix(dist(locmat)) # distance matrix (Euclidean)
covmat = sigmasq*exp(-distmat/phi) # covariance matrix w/ expon cov
muvec= beta*loc
simGP=mvrnorm(n = 1, muvec, covmat) #, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
plot(loc,simGP, col="red", type="l", xlab="location")
simGP=mvrnorm(n = M, muvec, covmat) #, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)

dim(simGP) ## M rows (replicates) each of 30-dimensional multivariate normal
sum(simGP[,3]>simGP[,4]^2 + simGP[,5])/M

### approximate P()

## x = grf(1000, grid = "irreg", nx, ny, xlims = c(0, 1), ylims = c(0, 1), borders, nsim = 1, cov.model = "matern", kappa = 0.5,nugget = 0, lambda = 1, mean = 0)
## x = grf(1000, cov.pars=c(1, .25), grid = "irreg")
## x = grf(1000, cov.pars=c(1, .25), grid = "irreg")
## xcoord = c(runif(500, 0.4,0.45), runif(500, 0,1))
## ycoord =  c(runif(500, 0.7,0.75), runif(500, 0,1))
## x = grf(1000, cov.pars=c(1, .25), grid =cbind(xcoord, ycoord))
## A=rnorm(10)
## B=(A)%*%t(A)
## system.time((C=chol(B)))

