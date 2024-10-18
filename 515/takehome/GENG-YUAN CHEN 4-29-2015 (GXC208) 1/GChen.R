################################################################################
### Problem 1
################################################################################
# clean memory, work space, and graphics
rm(list = ls())
cat("\014")
graphics.off()

## Fixed parameters
beta.0 = 5
sigma = 1
lambda = 0.4

## Tunable parameters
# Sample Size
N = 50000 # Run for 30 sec
# Tuning Parameter for proposal
tau = 0.3
# Random Seed
Seed = 123456
set.seed(Seed)
# Initial Condition
IC = rnorm(1, mean=7.3, sd=0.3)

# load data and functions
input = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
X = input[,1]
Y = input[,2]
n = length(X)

### ================= External script: batchmean===================
## This will return the mean of the vector, along with an estimate of MCMC standard error based
## on the consistent batchmeans estimator
bm <- function(vals,bs="sqroot",warn=FALSE)
{
  N <- length(vals)
  if (N<1000)
  {
    if (warn) # if warning
      cat("WARNING: too few samples (less than 1000)\n")
    if (N<10)
      return(NA)
  }
  
  if (bs=="sqroot") 
  {
    b <- floor(sqrt(N)) # batch size
    a <- floor(N/b) # number of batches
  }
  else
    if (bs=="cuberoot") 
    {
      b <- floor(N^(1/3)) # batch size
      a <- floor(N/b) # number of batches
    }
  else # batch size provided
  {
    stopifnot(is.numeric(bs))  
    b <- floor(bs) # batch size
    if (b > 1) # batch size valid
      a <- floor(N/b) # number of batches
    else
      stop("batch size invalid (bs=",bs,")")
  }
  
  Ys <- sapply(1:a,function(k) return(mean(vals[((k-1)*b+1):(k*b)])))
  
  muhat <- mean(Ys)
  sigmahatsq <- b*sum((Ys-muhat)^2)/(a-1)
  
  bmse <- sqrt(sigmahatsq/N)
  
  return(list(est=muhat,se=bmse))
}

## output: plot of estimate over time (increasing sample size)
estvssamp = function(samp, g=mean, plotname="mean estimates"){
  if (length(samp)<100)
    batchsize = 1
  else
    batchsize = length(samp)%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp),by=batchsize)){
    est = c(est, g(samp[1:i]))
  }  
  plot(seq(batchsize,length(samp),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MC estimate")
}

## output: plot of estimate over time (increasing sample size)
mcsevssamp = function(samp, g=sd, plotname="MEse estimates"){
  if (length(samp)<100)
    batchsize = 1
  else
    batchsize = length(samp)%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp),by=batchsize)){
    est = c(est, g(samp[1:i])/sqrt(i))
  }
  
  plot(seq(batchsize,length(samp),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MCse")
}

## ESS = T/kappa  where kappa (the `autocorrelation time' for the sample) = 1 + 2 sum of all lag auto-correlations
ess = function(outp,imselags=TRUE)
{
  if (imselags) # truncate number of lags based on imse approach
  {
    chainACov <- acf(outp,type="covariance",plot = FALSE)$acf ## USE AUTOCOVARIANCES
    ACovlen <- length(chainACov)
    gammaACov <- chainACov[1:(ACovlen-1)]+chainACov[2:ACovlen]
    
    m <- 1
    currgamma <- gammaACov[1]
    k <- 1
    while ((k<length(gammaACov)) && (gammaACov[k+1]>0) && (gammaACov[k]>=gammaACov[k+1]))
      k <- k +1
    cat("truncated after ",k," lags\n")
    if (k==length(gammaACov)) # added up until the very last computed autocovariance
      cat("WARNING: may need to compute more autocovariances/autocorrelations for ess\n")
    
    chainACorr = acf(outp,type="correlation",plot = FALSE)$acf ## USE AUTOCORRELATIONS
    if (k==1)
      ACtime = 1
    else
      ACtime <- 1 + 2*sum(chainACorr[2:k])  # add autocorrelations up to lag determined by imse
  }
  else
  {
    chainACorr = acf(outp,type="correlation",plot = FALSE)$acf ## USE AUTOCORRELATIONS
    ACtime <- 1 + 2*sum(chainACorr[-c(1)])
  }
  
  return(length(outp)/ACtime)
}

### ================= External script: EMG distribution===================
## pdf of EMG
dexpgauss<- function (x, mu = 0, sigma = 1, lambda = 1, log = FALSE)
{
  l <- max(length(x), length(mu), length(sigma), length(lambda))
  x <- rep(x, times = ceiling(l/length(x)), length.out = l)
  mu <- rep(mu, times = ceiling(l/length(mu)), length.out = l)
  sigma <- rep(sigma, times = ceiling(l/length(sigma)), length.out = l)
  lambda <- rep(lambda, times = ceiling(l/length(lambda)),
                length.out = l)
  if (min(sigma) <= 0) {
    stop("Sigma must be greater than zero")
  }
  if (min(lambda) <= 0) {
    stop("Lambda must be greater than zero")
  }
  erfc <- pnorm((mu + lambda * sigma * sigma - x)/sigma, lower.tail = FALSE,
                log.p = log)
  if (log) {
    result <- lambda/2 * (2 * mu + lambda * sigma * sigma -
                            2 * x) + Re(erfc) + log(lambda)
  }
  else {
    result <- exp(lambda/2 * (2 * mu + lambda * sigma * sigma -
                                2 * x)) * Re(erfc) * lambda
  }
  result[is.nan(result)] <- 0
  result
}

## EMG random variate generation
rexpgauss <- function(n, mu, sigma, lambda)
{
  samp <- rnorm(n, mu, sigma)+rexp(n,lambda) 
  return(samp)
}

### ==================== External script: M-H Drawer======================
# unversival parameters
mu.beta = 0
sigma.beta = 10

# define prior distribution, h ~ N(mean = mu.beta, sd = sigma.beta)
h <- function(x){ return(dnorm(x, mean = mu.beta, sd = sigma.beta)) }

# define transition kernal: N(mean = q.c, sd = tau)
q.r <- function(x,q.c,tau){ return(rnorm(x, mean = q.c, sd = tau)) }

# define log-likelihood function, g = log.L(beta) = sum(log.f)
g <- function(beta){ 
  log.f = dexpgauss(Y, mu = beta.0+beta*X, sigma = sigma, lambda = lambda, log = TRUE)
  return(sum(log.f))
}

# Metropolis-Hasting: get alpha|Y
MetropolisHastingsDrawer <- function(N,tau,IC){
  # define Judge uniform rand var
  J = runif(N,min=0,max=1)

  B = rep(NA,N) # for beta.1
  
  # Initial guess
  B[1] = IC # beta.1
  log.Pi = g(B[1])+log(h(B[1])) # pi is proportional to likelihood * prior
  
  for (i in 2:N){
    # draw beta.star from q
    b.star = q.r(1,B[i-1],tau)
    # get log.pi(beta.star)    
    log.pi.star = g(b.star)+log(h(b.star))
    
    # judge: accept or not?
    P.accept = min(exp(log.pi.star-log.Pi),1)
    if (J[i] < P.accept){ # accept
      B[i] = b.star
      log.Pi = log.pi.star
    }else{ # reject
      B[i] = B[i-1]
    }
  }
  return(B)
}

### ================================= Main Script =================================
## (a) Metropolis-Hasting Draw: MetropolisHastingDrawer(N,tau,IC,Seed)
B = MetropolisHastingsDrawer(N,tau,IC)

# (b) Calculate E(beta|X,Y) and its MCMCse
bm(B)

# (c) 95% Confidence Interval
quantile(B,c(0.025,0.975))
ess(B)

# (d) plot kernel density of beta
plot(density(B), main="(a). Density of beta (ESS=5231)")
plot(X,Y,xlim=c(0,max(X)),ylim=c(4,max(Y)), main="(b). beta.hat=mean(beta)")
abline(c(5,mean(B)),col="red") 
y.hat = mean(B)*X + 5
residual = Y-y.hat
noise = rexpgauss(length(Y), 0, sigma, lambda)
plot(density(residual), main = "(c). Fitted Residuals", xlab = "Residual",xlim =c(-5,15))
plot(density(noise), main = "(d). Simulated Noise", xlab = "Residual",xlim =c(-5,15))

# (e) Verification
estvssamp(B,mean,"(a). E(beta)") # E(beta) vs sample size
mcsevssamp(B,sd,"(b). MCSE(beta.hat)") # MCSE vs sample size
estvssamp(B,sd,"(c). SD(beta)") # SD(beta) vs sample size
acf(B, main = "(d). Memory of beta")

################################################################################
### Problem 2
################################################################################
# clean memory, work space, and graphics
rm(list = ls())
cat("\014")
graphics.off()

## Fixed parameters
sigma = 1

## Tunable parameters
# Sample Size
N = 500000 # Run for 5 min
# Tuning Parameter for proposals
tau.b0 = 1
tau.b1 = 1
tau.l = 0.01
# Random Seed
Seed = 123456
set.seed(Seed)
# Initial Conditions
beta.0 = rnorm(1,mean=2.3,sd=0.12)
beta.1 = rnorm(1,mean=3.5,sd=0.18)
lambda = rnorm(1,mean=0.79,sd=0.053)

# load data and functions
input = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
X = input[,1]
Y = input[,2]
n = length(X)

### ================= External script: batchmean===================
## This will return the mean of the vector, along with an estimate of MCMC standard error based
## on the consistent batchmeans estimator
bm <- function(vals,bs="sqroot",warn=FALSE)
{
  N <- length(vals)
  if (N<1000)
  {
    if (warn) # if warning
      cat("WARNING: too few samples (less than 1000)\n")
    if (N<10)
      return(NA)
  }
  
  if (bs=="sqroot") 
  {
    b <- floor(sqrt(N)) # batch size
    a <- floor(N/b) # number of batches
  }
  else
    if (bs=="cuberoot") 
    {
      b <- floor(N^(1/3)) # batch size
      a <- floor(N/b) # number of batches
    }
  else # batch size provided
  {
    stopifnot(is.numeric(bs))  
    b <- floor(bs) # batch size
    if (b > 1) # batch size valid
      a <- floor(N/b) # number of batches
    else
      stop("batch size invalid (bs=",bs,")")
  }
  
  Ys <- sapply(1:a,function(k) return(mean(vals[((k-1)*b+1):(k*b)])))
  
  muhat <- mean(Ys)
  sigmahatsq <- b*sum((Ys-muhat)^2)/(a-1)
  
  bmse <- sqrt(sigmahatsq/N)
  
  return(list(est=muhat,se=bmse))
}

## output: plot of estimate over time (increasing sample size)
estvssamp = function(samp, g=mean, plotname="mean estimates"){
  if (length(samp)<100)
    batchsize = 1
  else
    batchsize = length(samp)%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp),by=batchsize)){
    est = c(est, g(samp[1:i]))
  }  
  plot(seq(batchsize,length(samp),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MC estimate")
}

## output: plot of estimate over time (increasing sample size)
mcsevssamp = function(samp, g=sd, plotname="MEse estimates"){
  if (length(samp)<100)
    batchsize = 1
  else
    batchsize = length(samp)%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp),by=batchsize)){
    est = c(est, g(samp[1:i])/sqrt(i))
  }
  
  plot(seq(batchsize,length(samp),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MCse")
}

## ESS = T/kappa  where kappa (the `autocorrelation time' for the sample) = 1 + 2 sum of all lag auto-correlations
ess = function(outp,imselags=TRUE)
{
  if (imselags) # truncate number of lags based on imse approach
  {
    chainACov <- acf(outp,type="covariance",plot = FALSE)$acf ## USE AUTOCOVARIANCES
    ACovlen <- length(chainACov)
    gammaACov <- chainACov[1:(ACovlen-1)]+chainACov[2:ACovlen]
    
    m <- 1
    currgamma <- gammaACov[1]
    k <- 1
    while ((k<length(gammaACov)) && (gammaACov[k+1]>0) && (gammaACov[k]>=gammaACov[k+1]))
      k <- k +1
    cat("truncated after ",k," lags\n")
    if (k==length(gammaACov)) # added up until the very last computed autocovariance
      cat("WARNING: may need to compute more autocovariances/autocorrelations for ess\n")
    
    chainACorr = acf(outp,type="correlation",plot = FALSE)$acf ## USE AUTOCORRELATIONS
    if (k==1)
      ACtime = 1
    else
      ACtime <- 1 + 2*sum(chainACorr[2:k])  # add autocorrelations up to lag determined by imse
  }
  else
  {
    chainACorr = acf(outp,type="correlation",plot = FALSE)$acf ## USE AUTOCORRELATIONS
    ACtime <- 1 + 2*sum(chainACorr[-c(1)])
  }
  
  return(length(outp)/ACtime)
}

### ================= External script: EMG distribution===================
## pdf of EMG
dexpgauss<- function (x, mu = 0, sigma = 1, lambda = 1, log = FALSE)
{
  l <- max(length(x), length(mu), length(sigma), length(lambda))
  x <- rep(x, times = ceiling(l/length(x)), length.out = l)
  mu <- rep(mu, times = ceiling(l/length(mu)), length.out = l)
  sigma <- rep(sigma, times = ceiling(l/length(sigma)), length.out = l)
  lambda <- rep(lambda, times = ceiling(l/length(lambda)),
                length.out = l)
  if (min(sigma) <= 0) {
    stop("Sigma must be greater than zero")
  }
  if (min(lambda) <= 0) {
    stop("Lambda must be greater than zero")
  }
  erfc <- pnorm((mu + lambda * sigma * sigma - x)/sigma, lower.tail = FALSE,
                log.p = log)
  if (log) {
    result <- lambda/2 * (2 * mu + lambda * sigma * sigma -
                            2 * x) + Re(erfc) + log(lambda)
  }
  else {
    result <- exp(lambda/2 * (2 * mu + lambda * sigma * sigma -
                                2 * x)) * Re(erfc) * lambda
  }
  result[is.nan(result)] <- 0
  result
}

## EMG random variate generation
rexpgauss <- function(n, mu, sigma, lambda)
{
  samp <- rnorm(n, mu, sigma)+rexp(n,lambda) 
  return(samp)
}

### ==================== External script: M-H Drawer======================
# unversival parameters
mu.beta = 0
sigma.beta = 10
a.lambda = 0.01 # shape
b.lambda = 100 # scale

# define prior distribution, h ~ N(mean = mu.beta, sd = sigma.beta)
h.B0 <- function(x){ return(dnorm(x, mean = mu.beta, sd = sigma.beta)) }
h.B1 <- function(x){ return(dnorm(x, mean = mu.beta, sd = sigma.beta)) }
h.L <- function(x){ return(dgamma(x, shape = a.lambda, scale = b.lambda)) }
h <- function(beta.0,beta.1,lambda){return(h.B0(beta.0)*h.B1(beta.1)*h.L(lambda))}

# define transition kernal: N(mean = q.c, sd = tau)
q.r.B0 <- function(x,q.c,tau){ return(rnorm(x, mean = q.c, sd = tau)) }
q.r.B1 <- function(x,q.c,tau){ return(rnorm(x, mean = q.c, sd = tau)) }
q.r.L <- function(x,q.c,tau){ return(rnorm(x, mean = q.c, sd = tau)) }
q.d.L <- function(x,q.c,tau){ return(dnorm(x, mean = q.c, sd = tau)) }

# define log-likelihood function, g = log.L(beta) = sum(log.f)
g <- function(beta.0,beta.1,lambda){ 
  log.f = dexpgauss(Y, mu = beta.0+beta.1*X, sigma = sigma, lambda = lambda, log = TRUE)
  return(sum(log.f))
}

# Metropolis-Hasting: get beta.0,beta.1,lambda|Y
MetropolisHastingDrawer <- function(N,beta.0,beta.1,lambda,tau.b0,tau.b1,tau.l){
  # define Judge uniform rand var
  J.B0 = runif(N,min=0,max=1)
  J.B1 = runif(N,min=0,max=1)
  J.L = runif(N,min=0,max=1)
  
  B0 = rep(NA,N) # for beta.0
  B1 = rep(NA,N) # for beta.1
  L = rep(NA,N) # for Lambda
  
  # Initial guess:
  B0[1] = beta.0 # beta.0
  B1[1] = beta.1 # beta.1
  L[1] = lambda # lambda
  # pi is proportional to likelihood * prior
  log.Pi.B0 = g(B0[1],B1[1],L[1])+log(h(B0[1],B1[1],L[1]))
  log.Pi.B1 = g(B0[1],B1[1],L[1])+log(h(B0[1],B1[1],L[1]))
  log.Pi.L = g(B0[1],B1[1],L[1])+log(h(B0[1],B1[1],L[1]))
  
  for (i in 2:N){
    ## Update beta.0
    # draw beta.0.star from q.B0
    b0.star = q.r.B0(1,B0[i-1],tau.b0)
    # get pi(beta.star)
    log.pi.b0.star = g(b0.star,B1[i-1],L[i-1])+log(h(b0.star,B1[i-1],L[i-1]))
    # judge: accept or not?
    P.accept = min(exp(log.pi.b0.star-log.Pi.B0),1)
    if (J.B0[i] < P.accept){ # accept
      B0[i] = b0.star
      log.Pi.B0 = log.pi.b0.star
    }else{ # reject
      B0[i] = B0[i-1]
    }
    
    ## Update beta.1
    # draw beta.1.star from q.B1
    b1.star = q.r.B1(1,B1[i-1],tau.b1)
    # get pi(beta.star)
    log.pi.b1.star = g(B0[i],b1.star,L[i-1])+log(h(B0[i],b1.star,L[i-1]))
    # judge: accept or not?
    P.accept = min(exp(log.pi.b1.star-log.Pi.B1),1)
    if (J.B1[i] < P.accept){ # accept
      B1[i] = b1.star
      log.Pi.B1 = log.pi.b1.star
    }else{ # reject
      B1[i] = B1[i-1]
    }  
    
    ## Update lambda
    # draw lambda.star from q.L
    l.star = q.r.L(1,L[i-1],tau.l)
    # get pi(beta.star)
    log.pi.l.star = g(B0[i],B1[i],l.star)+log(h(B0[i],B1[i],l.star))
    # get proposal ratio (if symmetry, log.prop = 0)
    log.prop = log(q.d.L(l.star,L[i-1],tau.l)/q.d.L(L[i-1],l.star,tau.l))
    # judge: accept or not?
    P.accept = min(exp(log.pi.l.star-log.Pi.L+log.prop),1)
    if (J.L[i] < P.accept){ # accept
      L[i] = l.star
      log.Pi.L = log.pi.l.star
    }else{ # reject
      L[i] = L[i-1]
    }  
  }
  return(cbind(B0,B1,L))
}

### ================================= Main Script =================================
# (a) Metropolis-Hasting Draw
Draw = MetropolisHastingDrawer(N,beta.0,beta.1,lambda,tau.b0,tau.b1,tau.l)
B0 = Draw[,1]
B1 = Draw[,2]
L = Draw[,3]

# (b) Calculate E(beta.0|X,Y), E(beta.1|X,Y), E(lambda|X,Y), their MCMCse, and 95% CI
bm(B0)
quantile(B0,c(0.025,0.975))
ess(B0)
bm(B1)
quantile(B1,c(0.025,0.975))
ess(B1)
bm(L)
quantile(L,c(0.025,0.975))
ess(L)

# (c) Approximate corr(beta.0,beta.1)
corr.hat = cor(B0,B1)
corr.hat

# (d) plot kernel density
plot(density(B0), main = "(a). Density of beta.0")
plot(density(B1), main = "(b). Density of beta.1")
plot(density(L), main = "(c). Density of lambda")
plot(X,Y,xlim=c(0,max(X)), main="(d). Estimated regression")
abline(c(mean(B0),mean(B1)), col="red") 

# (e) Verification
estvssamp(B0,mean,"(a). E(beta.0)") # E(beta.0) vs sample size
mcsevssamp(B0,sd,"(b). MCSE(beta.0)") # MCSE vs sample size
estvssamp(B0,sd,"(c). SD(beta.0)") # SD(beta.0) vs sample size
acf(B0, main = "(d). Memory of beta.0")

estvssamp(B1,mean,"(e). E(beta.1)") # E(beta.1) vs sample size
mcsevssamp(B1,sd,"(f). MCSE(beta.1)") # MCSE vs sample size
estvssamp(B1,sd,"(g). SD(beta.1)") # SD(beta.1) vs sample size
acf(B1, main = "(h). Memory of beta.1")

estvssamp(L,mean,"(i). E(lambda)") # E(lambda) vs sample size
mcsevssamp(L,sd,"(j). MCSE(Lambda)") # MCSE vs sample size
estvssamp(L,sd,"(k). SD(lambda)") # SD(lambda) vs sample size
acf(L, main = "(l). Memory of lambda")

################################################################################
### Problem 3
################################################################################
# clean memory, work space, and graphics
rm(list = ls())
cat("\014")
graphics.off()

# Fixed parameters
sigma = 1

# Tunable parameters
# Sample size
N = 500000 # Run for 5 min
# tuning parameter for proposal
tau.b0 = 1
tau.b1 = 1
tau.l = 1
# Seeds
Seed = 123456
set.seed(Seed)
# Initial Conditions
beta.0 = rnorm(1,0.15,0.15)
beta.1 = rnorm(1,2.5,0.25)
lambda = rnorm(1,0.16,0.006)

# load data and functions
input = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")
X = input[,1]
Y = input[,2]
n = length(X)

### ================= External script: batchmean===================
## This will return the mean of the vector, along with an estimate of MCMC standard error based
## on the consistent batchmeans estimator
bm <- function(vals,bs="sqroot",warn=FALSE)
{
  N <- length(vals)
  if (N<1000)
  {
    if (warn) # if warning
      cat("WARNING: too few samples (less than 1000)\n")
    if (N<10)
      return(NA)
  }
  
  if (bs=="sqroot") 
  {
    b <- floor(sqrt(N)) # batch size
    a <- floor(N/b) # number of batches
  }
  else
    if (bs=="cuberoot") 
    {
      b <- floor(N^(1/3)) # batch size
      a <- floor(N/b) # number of batches
    }
  else # batch size provided
  {
    stopifnot(is.numeric(bs))  
    b <- floor(bs) # batch size
    if (b > 1) # batch size valid
      a <- floor(N/b) # number of batches
    else
      stop("batch size invalid (bs=",bs,")")
  }
  
  Ys <- sapply(1:a,function(k) return(mean(vals[((k-1)*b+1):(k*b)])))
  
  muhat <- mean(Ys)
  sigmahatsq <- b*sum((Ys-muhat)^2)/(a-1)
  
  bmse <- sqrt(sigmahatsq/N)
  
  return(list(est=muhat,se=bmse))
}

## output: plot of estimate over time (increasing sample size)
estvssamp = function(samp, g=mean, plotname="mean estimates"){
  if (length(samp)<100)
    batchsize = 1
  else
    batchsize = length(samp)%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp),by=batchsize)){
    est = c(est, g(samp[1:i]))
  }  
  plot(seq(batchsize,length(samp),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MC estimate")
}

## output: plot of estimate over time (increasing sample size)
mcsevssamp = function(samp, g=sd, plotname="MEse estimates"){
  if (length(samp)<100)
    batchsize = 1
  else
    batchsize = length(samp)%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp),by=batchsize)){
    est = c(est, g(samp[1:i])/sqrt(i))
  }
  
  plot(seq(batchsize,length(samp),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MCse")
}

## ESS = T/kappa  where kappa (the `autocorrelation time' for the sample) = 1 + 2 sum of all lag auto-correlations
ess = function(outp,imselags=TRUE)
{
  if (imselags) # truncate number of lags based on imse approach
  {
    chainACov <- acf(outp,type="covariance",plot = FALSE)$acf ## USE AUTOCOVARIANCES
    ACovlen <- length(chainACov)
    gammaACov <- chainACov[1:(ACovlen-1)]+chainACov[2:ACovlen]
    
    m <- 1
    currgamma <- gammaACov[1]
    k <- 1
    while ((k<length(gammaACov)) && (gammaACov[k+1]>0) && (gammaACov[k]>=gammaACov[k+1]))
      k <- k +1
    cat("truncated after ",k," lags\n")
    if (k==length(gammaACov)) # added up until the very last computed autocovariance
      cat("WARNING: may need to compute more autocovariances/autocorrelations for ess\n")
    
    chainACorr = acf(outp,type="correlation",plot = FALSE)$acf ## USE AUTOCORRELATIONS
    if (k==1)
      ACtime = 1
    else
      ACtime <- 1 + 2*sum(chainACorr[2:k])  # add autocorrelations up to lag determined by imse
  }
  else
  {
    chainACorr = acf(outp,type="correlation",plot = FALSE)$acf ## USE AUTOCORRELATIONS
    ACtime <- 1 + 2*sum(chainACorr[-c(1)])
  }
  
  return(length(outp)/ACtime)
}

### ================= External script: EMG distribution===================
## pdf of EMG
dexpgauss<- function (x, mu = 0, sigma = 1, lambda = 1, log = FALSE)
{
  l <- max(length(x), length(mu), length(sigma), length(lambda))
  x <- rep(x, times = ceiling(l/length(x)), length.out = l)
  mu <- rep(mu, times = ceiling(l/length(mu)), length.out = l)
  sigma <- rep(sigma, times = ceiling(l/length(sigma)), length.out = l)
  lambda <- rep(lambda, times = ceiling(l/length(lambda)),
                length.out = l)
  if (min(sigma) <= 0) {
    stop("Sigma must be greater than zero")
  }
  if (min(lambda) <= 0) {
    stop("Lambda must be greater than zero")
  }
  erfc <- pnorm((mu + lambda * sigma * sigma - x)/sigma, lower.tail = FALSE,
                log.p = log)
  if (log) {
    result <- lambda/2 * (2 * mu + lambda * sigma * sigma -
                            2 * x) + Re(erfc) + log(lambda)
  }
  else {
    result <- exp(lambda/2 * (2 * mu + lambda * sigma * sigma -
                                2 * x)) * Re(erfc) * lambda
  }
  result[is.nan(result)] <- 0
  result
}

## EMG random variate generation
rexpgauss <- function(n, mu, sigma, lambda)
{
  samp <- rnorm(n, mu, sigma)+rexp(n,lambda) 
  return(samp)
}

### ==================== External script: M-H Drawer======================
# unversival parameters
offset = 5
mu.beta = 0
sigma.beta = 10
a.lambda = 0.01 # shape
b.lambda = 100 # scale
prob = 0.5 # equal population 

# define prior distribution, h ~ N(mean = mu.beta, sd = sigma.beta)
h.B0 <- function(x){ return(dnorm(x, mean = mu.beta, sd = sigma.beta)) }
h.B1 <- function(x){ return(dnorm(x, mean = mu.beta, sd = sigma.beta)) }
h.L <- function(x){ return(dgamma(x, shape = a.lambda, scale = b.lambda)) }
h <- function(beta.0,beta.1,lambda){return(h.B0(beta.0)*h.B1(beta.1)*h.L(lambda))}

## define transition kernal: N(mean = q.c, sd = tau)
# beta.0 ~ 2-Gaussian
# q.r.B0 <- function(x,q.c,tau){ return(rnorm(x,mean = ifelse(runif(1) < prob, q.c+offset, q.c-offset), sd = tau))}
# beta.0 ~ 1-Gaussian
q.r.B0 <- function(x,q.c,tau){ return(rnorm(x, mean = q.c, sd = tau)) }
# beta.1 ~ Normal
q.r.B1 <- function(x,q.c,tau){ return(rnorm(x, mean = q.c, sd = tau)) }
# lambda ~ exp (note that center = mean)
#q.r.L <- function(x,tau){ return(rexp(x, rate = 1/tau)) }
#q.d.L <- function(x,tau){ return(dexp(x, rate = 1/tau)) }
# lambda ~ Gamma(kappa,theta), center at kappa*theta, theta defined as tau.l, from input
q.r.L <- function(x,kappa,theta){ return(rgamma(x, shape = kappa, scale = theta))}
q.d.L <- function(x,kappa,theta){ return(dgamma(x, shape = kappa, scale = theta))}

# define log-likelihood function, g = log.L(beta) = sum(log.f)
g <- function(beta.0,beta.1,lambda){ 
  log.f = dexpgauss(Y, mu = beta.0+beta.1*X, sigma = sigma, lambda = lambda, log = TRUE)
  return(sum(log.f))
}

# Metropolis-Hasting: get beta.0,beta.1,lambda|Y
MetropolisHastingDrawer <- function(N,beta.0,beta.1,lambda,tau.b0,tau.b1,tau.l){
  # define Judge uniform rand var
  J.B0 = runif(N,min=0,max=1)
  J.B1 = runif(N,min=0,max=1)
  J.L = runif(N,min=0,max=1)
  
  B0 = rep(NA,N) # for beta.0
  B1 = rep(NA,N) # for beta.1
  L = rep(NA,N) # for Lambda
  
  # Initial guess:
  B0[1] = beta.0 # beta.0
  B1[1] = beta.1 # beta.1
  L[1] = lambda # lambda
  # pi is proportional to likelihood * prior
  log.Pi.B0 = g(B0[1],B1[1],L[1])+log(h(B0[1],B1[1],L[1]))
  log.Pi.B1 = g(B0[1],B1[1],L[1])+log(h(B0[1],B1[1],L[1]))
  log.Pi.L = g(B0[1],B1[1],L[1])+log(h(B0[1],B1[1],L[1]))
  
  for (i in 2:N){
    ## Update beta.0
    # draw beta.0.star from q.B0
    b0.star = q.r.B0(1,B0[i-1],tau.b0)
    # get pi(beta.star)
    log.pi.b0.star = g(b0.star,B1[i-1],L[i-1])+log(h(b0.star,B1[i-1],L[i-1]))
    # judge: accept or not?
    P.accept = min(exp(log.pi.b0.star-log.Pi.B0),1)
    if (J.B0[i] < P.accept){ # accept
      B0[i] = b0.star
      log.Pi.B0 = log.pi.b0.star
    }else{ # reject
      B0[i] = B0[i-1]
    }
    
    ## Update beta.1
    # draw beta.1.star from q.B1
    b1.star = q.r.B1(1,B1[i-1],tau.b1)
    # get pi(beta.star)
    log.pi.b1.star = g(B0[i],b1.star,L[i-1])+log(h(B0[i],b1.star,L[i-1]))
    # judge: accept or not?
    P.accept = min(exp(log.pi.b1.star-log.Pi.B1),1)
    if (J.B1[i] < P.accept){ # accept
      B1[i] = b1.star
      log.Pi.B1 = log.pi.b1.star
    }else{ # reject
      B1[i] = B1[i-1]
    }  
    
    ## Update lambda
    # draw lambda.star from q.L
    l.star = q.r.L(1,L[i-1]/tau.l,tau.l)
    # get pi(beta.star)
    log.pi.l.star = g(B0[i],B1[i],l.star)+log(h(B0[i],B1[i],l.star))
    # get proposal ratio (if symmetry, log.ratio = 0)
    log.prop = log(q.d.L(l.star,L[i-1]/tau.l,tau.l)/q.d.L(L[i-1],l.star/tau.l,tau.l))
    # judge: accept or not?
    P.accept = min(exp(log.pi.l.star-log.Pi.L+log.prop),1)
    if (J.L[i] < P.accept){ # accept
      L[i] = l.star
      log.Pi.L = log.pi.l.star
    }else{ # reject
      L[i] = L[i-1]
    }  
  }
  return(cbind(B0,B1,L))
}

### ================================= Main Script =================================
# Metropolis-Hasting Draw
Draw = MetropolisHastingDrawer(N,beta.0,beta.1,lambda,tau.b0,tau.b1,tau.l)
B0 = Draw[,1]
B1 = Draw[,2]
L = Draw[,3]

# (a) Calculate E(beta.0|X,Y), E(beta.1|X,Y), E(lambda|X,Y), their MCMCse, and 95% CI
bm(B0)
quantile(B0,c(0.025,0.975))
ess(B0)
bm(B1)
quantile(B1,c(0.025,0.975))
ess(B1)
bm(L)
quantile(L,c(0.025,0.975))
ess(L)

# (b) plot kernel density
plot(density(B0), main = "(a) Density of beta.0")
plot(density(B1), main = "(b) Density of beta.1")
plot(density(L), main = "(c) Density of lambda")
plot(X,Y,xlim=c(0,max(X)), main="(d). Estimated regression")
abline(c(mean(B0),mean(B1)), col="red") 

# (c) Model Comparison
estvssamp(B0,mean,"(a). E(beta.0)") # E(beta.0) vs sample size
mcsevssamp(B0,sd,"(b). MCSE(beta.0)") # MCSE vs sample size
estvssamp(B0,sd,"(c). SD(beta.0)") # SD(beta.0) vs sample size
acf(B0, main = "(d). Memory (beta.0)")

estvssamp(B1,mean,"(a). E(beta.1)") # E(beta.1) vs sample size
mcsevssamp(B1,sd,"(b). MCSE(beta.1)") # MCSE vs sample size
estvssamp(B1,sd,"(c). SD(beta.1)") # SD(beta.1) vs sample size
acf(B1, main = "(d). Memory (beta.1)")

estvssamp(L,mean,"(a). E(lambda)") # E(lambda) vs sample size
mcsevssamp(L,sd,"(b). MCSE(Lambda)") # MCSE vs sample size
estvssamp(L,sd,"(c). SD(lambda)") # SD(lambda) vs sample size
acf(L, main = "(d). Memory (lambda)")


