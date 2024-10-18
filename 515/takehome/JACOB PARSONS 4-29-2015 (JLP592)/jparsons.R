#####################################################
#Take Home Final
#
#Problem 1
#
#Jacob Parsons
####################################################

#Include Batch Means
source("http://sites.stat.psu.edu/~mharan/batchmeans.R")

#################################################
## pdf of EMG
## set log=TRUE to obtain pdf in log-scale
## Taken from provided code
#################################################
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

#####################################################################################
#The unormalized posterior density function of Beta_1 given the observed data points
#All parameters other than beta one are treated as constants.
######################################################################################
posterior <- function(beta, data, log = TRUE)
{
   
  mu = 5+beta*data[,1]
  
  logLikelihood = sum(dexpgauss(data[,2], mu, 1, .4, log = TRUE))
  logPrior = dnorm(beta, mean = 0, sd = 10 ,log = TRUE)
  
  logPosterior = logLikelihood + logPrior
  
  if(log == TRUE)
  {
    result <- logPosterior
  }
  else
  {
    result <- exp(logPosterior)
  }
  
}


#A function that generates the MC used to estimate the posterior distribution
#Takes the data for the regression, the tuning parameter tau, and the point
#at which to start the chain. 
getChain <- function(data, starting, tau = 1)
{
  #Effective Sample Size
  ESS = 0

  #Use starting as an initial value for the chain.
  chain = c(starting)

  #Sample until the chain is reasonably long
  while(ESS < 5000)
  {
    #Generate a candidate from the proposal distribution
    candidate = rnorm(1,chain[length(chain)],tau)
    
    #Calculate acceptance probability
    logratio = posterior(candidate, data) +  -  posterior(chain[length(chain)], data)

  
    #Accept with the proabability from above
    u = runif(1)
  
    if(log(u) < logratio)
    {
      chain = c(chain, candidate)
    }
  
    else
    {
      chain = c(chain, chain[length(chain)])
    }

    #The loop slows down significantly if ESS is checked every time.
    #update it once every 1000 Updates
    if(length(chain) %% 1000 == 0)
    {
      ESS = ess(chain)
    
    }
  }
  
  return(chain)
}

#Load in the data for the regression
data = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")

#Estimate the Posterior
chain1a = getChain(data, 7.34 , tau = .85)

#Get a point estimate and its assoiciated standard error
bm(chain1a)

#Get a 95% credible interval
quantile(chain1a, c(.025,.975))

#Plot an estimated density
plot(density(chain1a), xlab ="Beta", ylab = "Density", main ="Posterior Distribution")

#Other Plots

estvssamp(chain1a, g = imse, plotname = "MCMC Standard Error vs Sample Size")


chain1b = getChain(data, 5, tau = .85)
chain1c = getChain(data, 9, tau = .85)

estplot1 = estvssamp(chain1a, retval=TRUE)
estplot2 = estvssamp(chain1b, retval = TRUE)
estplot3 = estvssamp(chain1c, retval = TRUE)

plot(estplot1, type="l", main="Estimates versus sample size", ylim = c(7.20, 7.4))
lines(estplot2, lty=2)
lines(estplot3, lty=3)

plot(acf(chain1a, plot = FALSE), main = "Autocorrelation Function")


#####################################################
#Take Home Final
#
#Problem 2
#
#Jacob Parsons
####################################################


#####################################################################################
#The unormalized posterior density function of Beta_1 given the observed data points
#All parameters other than beta one are treated as constants.
######################################################################################
posterior2 <- function(beta0, beta1, lambda, data, log = TRUE)
{
  
  mu = beta0+beta1*data[,1]
  
  logLikelihood = sum(dexpgauss(data[,2], mu, 1, lambda, log = TRUE))
  logPrior = dnorm(beta0, mean = 0, sd = 10 ,log = TRUE) + dnorm(beta1, mean = 0, sd = 10 ,log = TRUE) + dgamma(lambda, shape = .01, scale = 100,log = TRUE)
  
  logPosterior = logLikelihood + logPrior
  
  if(log == TRUE)
  {
    result <- logPosterior
  }
  else
  {
    result <- exp(logPosterior)
  }
  
  return(result)
  
}

#A function that generates the MC used to estimate the posterior distribution
#Takes the data for the regression, the tuning parameter tau, and the point
#at which to start the chain. 

getChain2 <- function(data, starting, tau1 = 1, tau2 = 1)
{
  
  #Effective Sample Size
  ESS = c(0,0,0)
  
  #Use starting as an initial values for the chain.
  chainbeta0 = c(starting[1])
  chainbeta1 = c(starting[2])
  chainlambda = c(starting[3])
  
  #Sample until the chain is reasonably long
  #Updates one at a time
  while(min(ESS) < 5000)
  {
    #Update Beta0
    #Generate a candidate from the proposal distribution
    candidate = rnorm(1,chainbeta0[length(chainbeta0)],tau1)
    
    #Calculate acceptance probability
    logratio = posterior2(candidate, chainbeta1[length(chainbeta1)], chainlambda[length(chainlambda)], data) - posterior2(chainbeta0[length(chainbeta0)],chainbeta1[length(chainbeta1)], chainlambda[length(chainlambda)], data)
    
    #Accept with the proabability from above
    u = runif(1)
    
    if(log(u) < logratio)
    {
      chainbeta0 = c(chainbeta0, candidate)
    }
    
    else
    {
      chainbeta0 = c(chainbeta0, chainbeta0[length(chainbeta0)])
    }
    
    #Update Beta1
    #Generate a candidate from the proposal distribution
    candidate = rnorm(1,chainbeta1[length(chainbeta1)],tau2)
    
    #Calculate acceptance probability
    logratio = posterior2( chainbeta0[length(chainbeta0)], candidate, chainlambda[length(chainlambda)], data) - posterior2(chainbeta0[length(chainbeta0)],chainbeta1[length(chainbeta1)], chainlambda[length(chainlambda)], data)
    
    #Accept with the proabability from above
    u = runif(1)
    
    if(log(u) < logratio)
    {
      chainbeta1 = c(chainbeta1, candidate)
    }
    
    else
    {
      chainbeta1 = c(chainbeta1, chainbeta1[length(chainbeta1)])
    }
    
    #Update lambda
    proposal = rexp(1,1/chainlambda[length(chainlambda)])
    
    #Calculate Acceptance Probability
    logConditionalRatio = posterior2(chainbeta0[length(chainbeta0)], chainbeta1[length(chainbeta1)], proposal, data) - posterior2(chainbeta0[length(chainbeta0)],chainbeta1[length(chainbeta1)], chainlambda[length(chainlambda)], data)
    logCorrectionRatio = dexp(proposal, 1/chainlambda[length(chainlambda)], log = TRUE) - dexp(chainlambda[length(chainlambda)], 1/proposal, log = TRUE)
    
    logratio = logConditionalRatio + logCorrectionRatio
    
    #Accept with the proabability from above
    u = runif(1)
    
    if(log(u) < logratio)
    {
      chainlambda = c(chainlambda, proposal)
    }
    
    else
    {
      chainlambda = c(chainlambda, chainlambda[length(chainlambda)])
    }
    
    
    #The loop slows down significantly if ESS is checked every time.
    #update it once every 1000 Updates
    
    if(length(chainbeta0) %% 10000 == 0)
    {
      ESS = c(ess(chainbeta0), ess(chainbeta1), ess(chainlambda))
    }
  }
  
  return(cbind(chainbeta0,chainbeta1,chainlambda))
}


#Read data
data2 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")

chain2a = getChain2(data2, c(2.289,3.4631,.7841), .8, .8)
chain2b = getChain2(data2, c(3.289,4.4631,.9841), .8, .8)
chain2c = getChain2(data2, c(1.289,2.4631,.5841), .8, .8)

#Get estimates and Standard error
bmmat(chain2a)

#Get a credible interval for each parameter
quantile(chain2a[,1], c(.025,.975))
quantile(chain2a[,2], c(.025,.975))
quantile(chain2a[,3], c(.025,.975))

#densities for each parameter
plot(density(chain2a[,1]), main = "Approx. Density of Beta0", xlim = c(1.9, 2.8))
plot(density(chain2a[,2]), main = "Approx. Density of Beta1", xlim = c(2.9, 4))
plot(density(chain2a[,3]), main = "Approx. Density of Lambda", xlim = c(.6, 1))

#Get an estimate of correlation between beta0 and beta1

cor(chain2a[,1], chain2a[,2])

#Plots

estvssamp(chain2a[,1], g = imse, plotname = "MCMC Standard Error vs Sample Size: Beta 0")
estvssamp(chain2a[,2], g = imse, plotname = "MCMC Standard Error vs Sample Size: Beta 1")
estvssamp(chain2a[,3], g = imse, plotname = "MCMC Standard Error vs Sample Size: Lambda")

estplot1b0 = estvssamp(chain2a[,1], retval=TRUE)
estplot1b1 = estvssamp(chain2a[,2], retval=TRUE)
estplot1lam = estvssamp(chain2a[,3], retval=TRUE)

estplot2b0 = estvssamp(chain2b[,1], retval = TRUE)
estplot2b1 = estvssamp(chain2b[,2], retval = TRUE)
estplot2lam = estvssamp(chain2b[,3], retval = TRUE)

estplot3b0 = estvssamp(chain2c[,1], retval = TRUE)
estplot3b1 = estvssamp(chain2c[,2], retval = TRUE)
estplot3lam = estvssamp(chain2c[,3], retval = TRUE)

plot(estplot1b0, type="l", main="Estimates versus sample size: Beta0")
lines(estplot2b0, lty=2)
lines(estplot3b0, lty=3)

plot(estplot1b1, type="l", main="Estimates versus sample size: Beta1")
lines(estplot2b1, lty=2)
lines(estplot3b1, lty=3)

plot(estplot1lam, type="l", main="Estimates versus sample size: Lambda")
lines(estplot2lam, lty=2)
lines(estplot3lam, lty=3)

plot(acf(chain2a[,1]), main = "Autocorrelation Function: Beta0")
plot(acf(chain2b[,2]), main = "Autocorrelation Function: Beta1")
plot(acf(chain2b[,3]), main = "Autocorrelation Function: Lambda")

#####################################################
#Take Home Final
#
#Problem 3
#
#Jacob Parsons
####################################################

#####################################################################################
#The unormalized posterior density function of Beta_1 given the observed data points
#All parameters other than beta one are treated as constants.
######################################################################################
posterior3 <- function(beta0, beta1, lambda, data, log = TRUE)
{
  
  mu = beta0+beta1*data[,1]
  
  logLikelihood = sum(dexpgauss(data[,2], mu, 1, lambda, log = TRUE))
  logPrior = dnorm(beta0, mean = 0, sd = 10 ,log = TRUE) + dnorm(beta1, mean = 0, sd = 10 ,log = TRUE) + dgamma(lambda, shape = .01, scale = 100,log = TRUE)
  
  logPosterior = logLikelihood + logPrior
  
  if(log == TRUE)
  {
    result <- logPosterior
  }
  else
  {
    result <- exp(logPosterior)
  }
  
  return(result)
  
}

#A function that generates the MC used to estimate the posterior distribution
#Takes the data for the regression, the tuning parameter tau, and the point
#at which to start the chain. 

getChain3 <- function(data, starting, tau1 = 1, tau2 = 1)
{
  
  #Effective Sample Size
  ESS = c(0,0,0)
  
  #Use starting as an initial values for the chain.
  chainbeta0 = c(starting[1])
  chainbeta1 = c(starting[2])
  chainlambda = c(starting[3])
  
  #Sample until the chain is reasonably long
  #Updates one at a time
  while(min(ESS) < 5000)
  {
    #Update Beta0
    #Generate a candidate from the proposal distribution
    candidate = rnorm(1,chainbeta0[length(chainbeta0)],tau1)
    
    #Calculate acceptance probability
    logratio = posterior3(candidate, chainbeta1[length(chainbeta1)], chainlambda[length(chainlambda)], data) - posterior3(chainbeta0[length(chainbeta0)],chainbeta1[length(chainbeta1)], chainlambda[length(chainlambda)], data)
    
    #Accept with the proabability from above
    u = runif(1)
    
    if(log(u) < logratio)
    {
      chainbeta0 = c(chainbeta0, candidate)
    }
    
    else
    {
      chainbeta0 = c(chainbeta0, chainbeta0[length(chainbeta0)])
    }
    
    #Update Beta1
    #Generate a candidate from the proposal distribution
    candidate = rnorm(1,chainbeta1[length(chainbeta1)],tau2)
    
    #Calculate acceptance probability
    logratio = posterior3( chainbeta0[length(chainbeta0)], candidate, chainlambda[length(chainlambda)], data) - posterior3(chainbeta0[length(chainbeta0)],chainbeta1[length(chainbeta1)], chainlambda[length(chainlambda)], data)
    
    #Accept with the proabability from above
    u = runif(1)
    
    if(log(u) < logratio)
    {
      chainbeta1 = c(chainbeta1, candidate)
    }
    
    else
    {
      chainbeta1 = c(chainbeta1, chainbeta1[length(chainbeta1)])
    }
    
    #Update lambda
    proposal = rexp(1,1/chainlambda[length(chainlambda)])
    
    #Calculate Acceptance Probability
    logConditionalRatio = posterior3(chainbeta0[length(chainbeta0)], chainbeta1[length(chainbeta1)], proposal, data) - posterior3(chainbeta0[length(chainbeta0)],chainbeta1[length(chainbeta1)], chainlambda[length(chainlambda)], data)
    logCorrectionRatio = dexp(proposal, 1/chainlambda[length(chainlambda)], log = TRUE) - dexp(chainlambda[length(chainlambda)], 1/proposal, log = TRUE)
    
    logratio = logConditionalRatio + logCorrectionRatio
    
    #Accept with the proabability from above
    u = runif(1)
    
    if(log(u) < logratio)
    {
      chainlambda = c(chainlambda, proposal)
    }
    
    else
    {
      chainlambda = c(chainlambda, chainlambda[length(chainlambda)])
    }
    
    
    #The loop slows down significantly if ESS is checked every time.
    #update it once every 1000 Updates
    
    if(length(chainbeta0) %% 40000 == 0)
    {
      ESS = c(ess(chainbeta0), ess(chainbeta1), ess(chainlambda))
    }
  }
  
  return(cbind(chainbeta0,chainbeta1,chainlambda))
}


#Read data
data3 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")

#Generate Chains
chain3a = getChain3(data3, c(.127,2.465,.1598), .7, .7)
chain3b = getChain3(data3, c(6,10,5), .7, .7)
chain3c = getChain3(data3, c(7, 5, 1), .7, .7)

#Get estimates and Standard error
bmmat(chain3a)

#Get a credible interval for each parameter
quantile(chain3a[,1], c(.025,.975))
quantile(chain3a[,2], c(.025,.975))
quantile(chain3a[,3], c(.025,.975))

#densities for each parameter
plot(density(chain3a[,1]), main = "Approx. Density of Beta0", xlim = c(-.3, .6))
plot(density(chain3a[,2]), main = "Approx. Density of Beta1", xlim = c(1.7, 3.2))
plot(density(chain3a[,3]), main = "Approx. Density of Lambda", xlim = c(.145, .175))


#Plots

estvssamp(chain3a[,1], g = imse, plotname = "MCMC Standard Error vs Sample Size: Beta 0")
estvssamp(chain3a[,2], g = imse, plotname = "MCMC Standard Error vs Sample Size: Beta 1")
estvssamp(chain3a[,3], g = imse, plotname = "MCMC Standard Error vs Sample Size: Lambda")

estplot1b0 = estvssamp(chain3a[,1], retval=TRUE)
estplot1b1 = estvssamp(chain3a[,2], retval=TRUE)
estplot1lam = estvssamp(chain3a[,3], retval=TRUE)

estplot2b0 = estvssamp(chain3b[,1], retval = TRUE)
estplot2b1 = estvssamp(chain3b[,2], retval = TRUE)
estplot2lam = estvssamp(chain3b[,3], retval = TRUE)

estplot3b0 = estvssamp(chain3c[,1], retval = TRUE)
estplot3b1 = estvssamp(chain3c[,2], retval = TRUE)
estplot3lam = estvssamp(chain3c[,3], retval = TRUE)

plot(estplot1b0, type="l", main="Estimates versus sample size")
lines(estplot2b0, lty=2)
lines(estplot3b0, lty=3)

plot(estplot1b1, type="l", main="Estimates versus sample size")
lines(estplot2b1, lty=2)
lines(estplot3b1, lty=3)

plot(estplot1lam, type="l", main="Estimates versus sample size")
lines(estplot2lam, lty=2)
lines(estplot3lam, lty=3)

plot(acf(chain3a[,1]), main = "Autocorrelation Function: Beta0")
plot(acf(chain3b[,2]), main = "Autocorrelation Function: Beta1")
plot(acf(chain3b[,3]), main = "Autocorrelation Function: Lambda")
