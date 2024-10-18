########################################
## generate a covariance matrix
## with structure sigmaI + KK'
## K is NxM
########################################
N=3000
M=10
sigma=0.2
Kmat=matrix(rnorm(N*M), N, M)
M=sigma*diag(rep(1,N)) + Kmat%*%t(Kmat)
system.time((solve(M)))
