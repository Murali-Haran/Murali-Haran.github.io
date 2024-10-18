source("http://www.stat.psu.edu/~mharan/batchmeans.R")
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

## function to plot pdf
## E.g. plotdexpgauss(-4,6,0,1,1)
## E.g. plotdexpgauss(-4,6,-2,1,1)
plotdexpgauss <- function(lowerlim, upperlim, mu, sigma, lambda, log=FALSE)
{
  xvals=seq(lowerlim, upperlim, length=100)
  tempfun = function(x) return(dexpgauss(x, mu,sigma, lambda, log))
  yvals=sapply(xvals, tempfun)
  if (log)
    plot(xvals, yvals, type="l", main=paste("log(pdf) of Exp Modified Gaussian (mu=",mu,", sigma=",sigma,", lambda=",lambda), xlab="x", ylab="")
  else
    plot(xvals, yvals, type="l", main=paste("Exp Modified Gaussian mu=",mu,"sigma=",sigma,",lambda=",lambda), xlab="x", ylab="")
}
  
## random variate generation
rexpgauss <- function(n, mu, sigma, lambda)
  {
    samp <- rnorm(n, mu, sigma)+rexp(n,lambda) 
    return(samp)
  }

#############################################
### SIMULATE DATA
#############################################
## n=500
## X = runif(n)
## BETA0=2; BETA1=4; SIGMA=1; LAMBDA=0.8
## set.seed(1)
## Y = BETA0 + BETA1*X + rnorm(n, 0, sd=SIGMA) + rexp(n, LAMBDA)
## testdat = list(height=X, age=Y, uncertainty=rep(1, n))
## sink("EMG2.dat")
## for (i in 1:n)
##   cat(X[i],Y[i],"\n")
## sink()

## n=100
## X = runif(n)
## BETA0=5; BETA1=7; SIGMA=1; LAMBDA=0.4
## set.seed(1)
## Y = BETA0 + BETA1*X + rnorm(n, 0, sd=SIGMA) + rexp(n, LAMBDA)
## testdat = list(height=X, age=Y, uncertainty=rep(1, n))
## sink("EMG1.dat")
## for (i in 1:n)
##   cat(X[i],Y[i],"\n")
## sink()
## plot(X,Y)

n=400
X = runif(n)
BETA0=3; BETA1=7; SIGMA=1; LAMBDA=0.4
set.seed(1)
Y = BETA0 + BETA1*X + rnorm(n, 0, sd=SIGMA) + rexp(n, LAMBDA)

n=700
X2 = runif(n)
BETA0=3; BETA1=5; SIGMA=1; LAMBDA=0.4
set.seed(1)
Y2 = BETA0 + BETA1*X2 + rnorm(n, 0, sd=SIGMA) + rexp(n, LAMBDA)

X = c(X, X2)
Y = c(Y, Y2)
n = length(X)
neworder = sample(seq(1,n), size=n, replace=FALSE)
X = X[neworder]
Y = Y[neworder]

testdat = list(height=X, age=Y, uncertainty=rep(1, n))
sink("EMG3.dat")
for (i in 1:n)
  cat(X[i],Y[i],"\n")
sink()
plot(X,Y)

####################################
## log likelihood function
####################################
logLike = function(m, b, k, cosmodat)
  {
    height=cosmodat$height
    age=cosmodat$age
    sigmavals=cosmodat$uncert
    n=length(cosmodat$height)
    
    logval = 0
    if (k<=0)
      stop("loglikelihood: Invalid m (",m,")\n")
    for (i in 1:n) # clear but inefficient way of calculating this
      {
        logval = logval + dexpgauss(x=age[i],mu=b + m*height[i], sigma=sigmavals[i], k, log=TRUE)
       ## cat("i=",i,"age=",age[i],"height=",height[i],"sigmavals=",sigmavals[i],"mu=",b + m*height[i],"k=",k,"log of dexpgauss=",dexpgauss(x=age[i],mu=b + m*height[i], sigma=sigmavals[i], k, log=TRUE),"\n")
       ##  cat("i=",i,"x,mu,sigma,lambda=",age[i],b + m*height[i],sigmavals[i],k,"log of dexpgauss=",dexpgauss(x=age[i],mu=b + m*height[i], sigma=sigmavals[i], k, log=TRUE),"\n")
       ##  cat("dexpgauss (log scale)=",dexpgauss(x=age[i],mu=b + m*height[i], sigma=sigmavals[i], k, log=TRUE),"\n")
      }
#    cat("logval=",logval,"\n")
    return(logval)
  }

########################################################################
## prior pdfs for the parameters
########################################################################
logpriorm = function(m)
  return(dgamma(m, shape=0.01, scale=100, log=TRUE))

logpriorb = function(b)
  return(dgamma(b, shape=0.01, scale=100, log=TRUE))

logpriork = function(k)
  return(dgamma(k, shape=0.01, scale=100, log=TRUE))
########################################################################
## Patrick's views:
#Best estimates by eye for this data set (Marble Hills, Bentley et
#al. 2010) would be intercept between 0 and 3 ka (thousand years), and
#a thinning rate of 500 m in not more than 15 ka, so a slope of 30
#yr/m. That is an upper bound on the slope, I believe; the thinning
#rate should not be less than 0.03.
## Summary:
## Intercept:between 0 and 3
## Slope: between 0 and 30
## K: not clear, between 0 and >100,000 (?)
####################################W####################################
## log posterior
########################################################################
logpost=function(m, b, k, cosmodat)
  {
    logval = logLike(m, b, k, cosmodat) + logpriorm(m) + logpriorb(b) + logpriork(k)
    return(logval)
  }

########################################################
## begin Metropolis-Hastings
########################################################
cosmoMCMC = function(NUMIT, cosmodat, taum=0.5, taub=0.5, tauk=0.5, init)
  {
    if (NUMIT<2)
      stop("Cannot have chain length=",NUMIT,"\n")
    mchain = matrix(NA, 3, NUMIT) # each column represents 1 state (iteration) of the Markov chain
    mchain[,1] = init # c(10,2000,10000) # initial values
    acc = rep(0,3) # to count acceptances for each parameter

    for (i in 2:NUMIT)
      {
#        cat("iteration =", i, "\n")
        currm=mchain[1,i-1]
        currb=mchain[2,i-1]
        currk=mchain[3,i-1]
        
        ## update m
        mchain[1,i] = currm # stay at current state unless proposal is accepted below
        prop=rnorm(1, currm, taum) # random walk proposal
        if (prop>0) # need k to be positive (else reject)
          {
            U=runif(1)
#            cat("logpost vals=",logpost(prop,currb,currk, cosmodat), logpost(currm,currb,currk, cosmodat), "\n")
            if (log(U) < (logpost(prop, currb, currk,cosmodat) - logpost(currm, currb,currk, cosmodat)))
              {
                acc[1] = acc[1]+1
                mchain[1,i] = prop # accept proposal
              }
          }

        ## update b
        mchain[2,i] = currb # stay
        prop=rnorm(1, currb, taub) # random walk proposal
        if (prop>0) # need k to be positive (else reject)
          {
            U=runif(1)
            if (log(U) < (logpost(currm,prop,currk,cosmodat) - logpost(currm, currb, currk, cosmodat)))
              {
                acc[2] = acc[2]+1
                mchain[2,i] = prop # accept proposal
              }
          }
#        mchain[2,i]=BETA0 # DON'T UPDATE
        ## update k
        mchain[3,i] = currk # stay at current state unless proposal is accepted below
        prop=rnorm(1, currk, tauk) # random walk proposal
#        cat("current k, proposed k",currk, prop,"\n")
        if (prop>0) # need k to be positive (else reject)
          {
            U = runif(1)
#            cat("at iteration ",i,"logMH=", logpost(currm,currb,prop,cosmodat),"-", logpost(currm,currb,currk,cosmodat),"\n")
            if (log(U) < (logpost(currm,currb,prop,cosmodat) - logpost(currm,currb,currk,cosmodat)))
              {
                acc[3] = acc[3]+1
                mchain[3,i] = prop # accept proposal
              }
          }
#        mchain[3,i]=LAMBDA # DON'T UPDATE
      }
    
    return(list(mchain=mchain, acc=acc))
  }

mhout = cosmoMCMC(NUMIT=1000, cosmodat=testdat, taub=0.5, taum=0.5, tauk=0.5, init=c(1, 1, 1))
apply(mhout$mchain,1, mean)
