##################################################
## define global parameters
bete0=5
lambda= 0.4
sigma=1
Realization=50000
startvalue=8
tau=10
#################################################
## pdf of EMG
## set log=TRUE to obtain pdf in log-scale
#################################################
dexpgauss<- function (x, mu = 0, sigma = 1, lambda = 1, log = TRUE)
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
## get data from the table
dat1 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
n=100   ## number of (yi,xi)
# First, we create a function to calculate log(pi):
logpi<- function(beteone)
{return (dexpgauss(dat1[,2], bete0+beteone*(dat1[,1]),
                   sigma, lambda = 1, log = TRUE)-beteone*beteone/200) }
# Here is a function to return a sample from the posterior:
samplePosterior <- function(tau=1) {
  bete1 <- rep(startvalue, Realization+1) # This will hold the theta values
  accepts <- 0 # Keep track of the number of acceptances
  # Here is the main loop:
  for(i in 1:Realization) {
    bete1Star <- rnorm(1, mean=bete1[i], sd=tau)
    u <- runif(1)
    if (bete1Star>0 && log(u) < logpi(bete1Star) - logpi(bete1[i])) {
      accepts <- accepts + 1
      bete1[i+1] <- bete1Star
    } else {
      bete1[i+1] <- bete1[i]
    }
  }
  cat("Acceptance rate: ", accepts / Realization)
  return(bete1)
}
bete1 <- samplePosterior(tau)
#####################################################################################################
##Q1-b
## Calculate the MCMCse
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
bm(bete1)
#################################################################################################
##Q1-c
quantile(bete1,c(0.025,0.975))
#############################################################################################
##Q1-d
plot (density(bete1))

##################################################
## define global parameters
bete0=5
lambda= 0.4
sigma=1
Realization=50000
startvalue=8
tau=10
Rea=seq (from=100, to=5000,by=100)
N=length(Rea)
#################################################
## pdf of EMG
## set log=TRUE to obtain pdf in log-scale
dexpgauss<- function (x, mu = 0, sigma = 1, lambda = 1, log = TRUE)
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
## get data from the table
dat1 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
n=100   ## number of (yi,xi)
# First, we create a function to calculate log(pi):
logpi<- function(beteone)
{return (dexpgauss(dat1[,2], bete0+beteone*(dat1[,1]),
                   sigma, lambda = 1, log = TRUE)-beteone*beteone/200) }
##################################################################################################
# Here is a function to return a sample mean from the posterior:
samplePosterior <- function(tau=1,Realization) 
{
  bete1 <- rep(startvalue, Realization+1) # This will hold the theta values
  accepts <- 0 # Keep track of the number of acceptances
  # Here is the main loop:
  for(i in 1:Realization) {
    bete1Star <- rnorm(1, mean=bete1[i], sd=tau)
    u <- runif(1)
    if (bete1Star>0 && log(u) < logpi(bete1Star) - logpi(bete1[i])) {
      accepts <- accepts + 1
      bete1[i+1] <- bete1Star
    } else {
      bete1[i+1] <- bete1[i]
    }
  }
  return(mean(bete1))
}
## plot the mean against Realization
mean=mapply(samplePosterior,tau,Rea )
plot(Rea,mean)
#############################################################################################
## plot the MCMCse against Realization
## Calculate the MCMCse
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
  
  return(bmse)
}
### main loop to get the MCMCse for each sample size
samplePos <- function(tau=1,Realization) 
{
  bete1 <- rep(startvalue, Realization+1) # This will hold the theta values
  accepts <- 0 # Keep track of the number of acceptances
  # Here is the main loop:
  for(i in 1:Realization) {
    bete1Star <- rnorm(1, mean=bete1[i], sd=tau)
    u <- runif(1)
    if (bete1Star>0 && log(u) < logpi(bete1Star) - logpi(bete1[i])) {
      accepts <- accepts + 1
      bete1[i+1] <- bete1Star
    } else {
      bete1[i+1] <- bete1[i]
    }
  }
  return(bm(bete1))
}
## calculate MCMCse for each sample size
MCMCse=mapply(samplePos,tau,Rea)
plot(Rea,MCMCse)
###############################################################################################
## plot the auto-correlation of 50000 realization and for tau=10
samplePosterior <- function(tau=1) {
  bete1 <- rep(startvalue, Realization+1) # This will hold the theta values
  accepts <- 0 # Keep track of the number of acceptances
  # Here is the main loop:
  for(i in 1:Realization) {
    bete1Star <- rnorm(1, mean=bete1[i], sd=tau)
    u <- runif(1)
    if (bete1Star>0 && log(u) < logpi(bete1Star) - logpi(bete1[i])) {
      accepts <- accepts + 1
      bete1[i+1] <- bete1Star
    } else {
      bete1[i+1] <- bete1[i]
    }
  }
  cat("Acceptance rate: ", accepts / Realization)
  return(bete1)
}
bete1 <- samplePosterior(tau)
acf(bete1)


###################################################################################################
###################################################################################################
##################################################################################################



##################################################
## define global parameters
sigma=1
Realization=100000
## start values and tau
stabete0=5
stalambda=0.2
stabete1=2
tau=10
#######################################################################
## pdf of EMG
## set log=TRUE to obtain pdf in log-scale
dexpgauss<- function (x, mu = 0, sigma = 1, lambda = 1, log = TRUE)
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
  if (min(lambda) <=0) {
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
## get data from the table
dat2 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
n=500   ## number of (yi,xi)

############################################################################
posteriorsampler = function(Realization=1000)
{
  ## Realization x 3 matrix to store Markov chain values
  ## each row corresponds to one of 3 parameters in order: lambda, bete1,bete0
  ## each column corresponds to a single state of the Markov chain
  mchain = matrix(1,Realization,3)
  ## calculate the accept rate for 3 parameters
  acclambda=0
  accbete1=0
  accbete0=0
  ## starting values for Markov chain
  mchain[1,] = c(stalambda,stabete1,stabete0)
  for (i in 2:Realization)
  {
    ## most upto date state for each parameter
    currlambda = mchain[i-1,1]
    currbete1 = mchain[i-1,2]
    currbete0 = mchain[i-1,3]
    ######################################################################################################    
    ## sample from full conditional distribution of lambda (Metropolis-Hastings update)
    # first, we create a function to calculate loglambda(pi):
    loglambdapi<- function(lambda, bete1,bete0)
    {return (dexpgauss(dat2[,2], bete0+bete1*(dat2[,1]),
                       sigma, lambda, log = TRUE)-0.99*log(lambda)-lambda/100) }
    # next,draw one sample at random from uniform distribution
    lambdastar <- runif(1,0,1)
    ## Metropolis accept-reject step (in log scale)
    logMHratio1 = loglambdapi(lambdastar,currbete1,currbete0)-loglambdapi(currlambda,currbete1,currbete0)
    logalpha1 = min(0,logMHratio1) # alpha = min(1,MHratio)
    # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    if (log(runif(1))<logalpha1) 
    {
      acclambda = acclambda + 1 # increment count of accepted proposals
      currlambda = lambdastar
    }
    
    #########################################################################################################    
    ## sample from full conditional distribution of bete1 (Metropolis update)
    # First, we create a function to calculate logbete1(pi):
    logbete1pi<- function(lambda, bete1,bete0)
    {return (dexpgauss(dat2[,2], bete0+bete1*(dat2[,1]),
                       sigma, lambda, log = TRUE)-bete1*bete1/200) }
    # draw one sample at random from normal distibution
    bete1star <- rnorm(1, mean=currbete1, sd=tau)
    ## Metropolis accept-reject step (in log scale)
    logMHratio2 = logbete1pi(currlambda,bete1star,currbete0)-logbete1pi(currlambda,currbete1,currbete0)
    logalpha2 = min(0,logMHratio2) # alpha = min(1,MHratio)
    # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    if (log(runif(1))<logalpha2) 
    {
      accbete1 = accbete1 + 1 # increment count of accepted proposals
      currbete1 = bete1star
    }
    
    
    ########################################################################################################   
    ## sample from full conditional distribution of bete0 (Metropolis update)
    # First, we create a function to calculate logbete1(pi):
    logbete0pi<- function(lambda, bete1,bete0)
    {return (dexpgauss(dat2[,2], bete0+bete1*(dat2[,1]),
                       sigma, lambda, log = TRUE)-bete0*bete0/200) }
    # draw one sample at random from normal distibution
    bete0star <- rnorm(1, mean=currbete0, sd=tau)
    ## Metropolis accept-reject step (in log scale)
    logMHratio3 = logbete0pi(currlambda,currbete1,bete0star)-logbete0pi(currlambda,currbete1,currbete0)
    logalpha3 = min(0,logMHratio3) # alpha = min(1,MHratio)
    # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    if (log(runif(1))<logalpha3) 
    {
      accbete0 = accbete0 + 1 # increment count of accepted proposals
      currbete0 = bete0star
    }
    ##########################################################################################
    ## update chain with new values
    mchain[i,] = c(currlambda,currbete1,currbete0)
  }
  
  cat("Markov chain algorithm ran for ",Realization,
      "iterations\n (accept.rate for lambda",acclambda/(Realization-1),")\n",
      "(accept.rate for bete1",accbete1/(Realization-1),")\n",
      "(accpet.rate for bete0",accbete0/(Realization-1),")\n")
  cat("Parameters are in order: lambda, bete1, bete0")
  return(mchain)
}
a=posteriorsampler(Realization)
######################################################################################
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
  
  return(list (est=muhat,se=bmse,Q=quantile(vals,c(0.025,0.975))))
}
cat("##############################################################################\n
    calculate the estimation and MCMCse and 95% quantile for lambda")
bm(a[,1])
cat("##############################################################################\n
    calculate the estimation and MCMCse and 95% quantile for bete1")
bm(a[,2])
cat("##############################################################################\n
    calculate the estimation and MCMCse and 95% quantile for bete0")
bm(a[,3])
#####################################################################################################
#Q2-c
cor(a[,2],a[,3])
################################################################################################
#Q2-d
plot(density(a[,1]),"density plot of lambda")
plot(density(a[,2]),"density plot of bete1")
plot(density(a[,3]),"density plot of bete0")
##################################################
## define global parameters
sigma=1
Realization=100000
## start values and tau
stabete0=-5
stalambda=1
stabete1=-2
tau=10
#######################################################################
## pdf of EMG
## set log=TRUE to obtain pdf in log-scale
dexpgauss<- function (x, mu = 0, sigma = 1, lambda = 1, log = TRUE)
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
## get data from the table
dat2 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
n=500   ## number of (yi,xi)

############################################################################
posteriorsampler = function(Realization=1000)
{
  ## Realization x 3 matrix to store Markov chain values
  ## each row corresponds to one of 3 parameters in order: lambda, bete1,bete0
  ## each column corresponds to a single state of the Markov chain
  mchain = matrix(NA,Realization,3)
  ## calculate the accept rate for 3 parameters
  acclambda=0
  accbete1=0
  accbete0=0
  ## starting values for Markov chain
  mchain[1,] = c(stalambda,stabete1,stabete0)
  for (i in 2:Realization)
  {
    ## most upto date state for each parameter
    currlambda = mchain[i-1,1]
    currbete1 = mchain[i-1,2]
    currbete0 = mchain[i-1,3]
######################################################################################################    
    ## sample from full conditional distribution of lambda (Metropolis-Hastings update)
    # first, we create a function to calculate loglambda(pi):
    loglambdapi<- function(lambda, bete1,bete0)
    {return (dexpgauss(dat2[,2], bete0+bete1*(dat2[,1]),
                       sigma, lambda, log = TRUE)-0.99*log(lambda)-lambda/100) }
    # next,draw one sample at random from uniform distribution
    lambdastar <- runif(1,0,1)
    ## Metropolis accept-reject step (in log scale)
    logMHratio1 = loglambdapi(lambdastar,currbete1,currbete0)-loglambdapi(currlambda,currbete1,currbete0)
    logalpha1 = min(0,logMHratio1) # alpha = min(1,MHratio)
    # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    if (log(runif(1))<logalpha1) 
    {
      acclambda = acclambda + 1 # increment count of accepted proposals
      currlambda = lambdastar
    }
    
    #########################################################################################################    
    ## sample from full conditional distribution of bete1 (Metropolis update)
    # First, we create a function to calculate logbete1(pi):
    logbete1pi<- function(lambda, bete1,bete0)
    {return (dexpgauss(dat2[,2], bete0+bete1*(dat2[,1]),
                       sigma, lambda, log = TRUE)-bete1*bete1/200) }
    # draw one sample at random from normal distibution
    bete1star <- rnorm(1, mean=currbete1, sd=tau)
    ## Metropolis accept-reject step (in log scale)
    logMHratio2 = logbete1pi(currlambda,bete1star,currbete0)-logbete1pi(currlambda,currbete1,currbete0)
    logalpha2 = min(0,logMHratio2) # alpha = min(1,MHratio)
    # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    if (log(runif(1))<logalpha2) 
    {
      accbete1 = accbete1 + 1 # increment count of accepted proposals
      currbete1 = bete1star
    }  
########################################################################################################   
    ## sample from full conditional distribution of bete0 (Metropolis update)
    # First, we create a function to calculate logbete1(pi):
    logbete0pi<- function(lambda, bete1,bete0)
    {return (dexpgauss(dat[,2], bete0+bete1*(dat[,1]),
                       sigma, lambda, log = TRUE)-bete0*bete0/200) }
    # draw one sample at random from normal distibution
    bete0star <- rnorm(1, mean=currbete0, sd=tau)
    ## Metropolis accept-reject step (in log scale)
    logMHratio3 = logbete0pi(currlambda,currbete1,bete0star)-logbete0pi(currlambda,currbete1,currbete0)
    logalpha3 = min(0,logMHratio3) # alpha = min(1,MHratio)
    # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    if (log(runif(1))<logalpha3) 
    {
      accbete0 = accbete0 + 1 # increment count of accepted proposals
      currbete0 = bete0star
    }
    ##########################################################################################
    ## update chain with new values
    mchain[i,] = c(currlambda,currbete1,currbete0)
  }
  
  return(mchain)
}

#############################################################################################
#get the autocorrelation of each estimation for 50000 realizations
a=posteriorsampler(Realization)
acf(a[,1])
acf(a[,2])
acf(a[,3])
###############################################################################################
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
  
  return(bmse)
}
###############################################################################################
##get the MCMCse and mean for differnet realization
Rea=seq (from=100, to=5000,by=100)
N=length(Rea)
mean=matrix(NA, N,3)
MCMCse=matrix(NA,N,3)
for (i in 1:N)
{
  a=posteriorsampler(Rea[i])
  mean[i,1]=mean(a[,1])
  MCMCse[i,1]=bm(a[,1])
  mean[i,2]=mean(a[,2])
  MCMCse[i,2]=bm(a[,2])
  mean[i,3]=mean(a[,3])
  MCMCse[i,3]=bm(a[,3])
}
##############################################################################################
plot(Rea, mean[,1])
plot(Rea,mean[,2])
plot(Rea,mean[,3])
###############################################################################################
plot(Rea,MCMCse[,1])
plot(Rea,MCMCse[,2])
plot(Rea,MCMCse[,3])



##############################################################################################
###############################################################################################
##############################################################################################

##################################################
## define global parameters
sigma=1
Realization=100000
## start values and tau
stabete0=0
stalambda=0.4
stabete1=-2
tau1=10
#######################################################################
## pdf of EMG
## set log=TRUE to obtain pdf in log-scale
dexpgauss<- function (x, mu = 0, sigma = 1, lambda = 1, log = TRUE)
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
## get data from the table
dat3 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")
n=1000   ## number of (yi,xi)

############################################################################
posteriorsampler = function(Realization=1000)
{
  ## Realization x 3 matrix to store Markov chain values
  ## each row corresponds to one of 3 parameters in order: lambda, bete1,bete0
  ## each column corresponds to a single state of the Markov chain
  mchain = matrix(1,Realization,3)
  ## calculate the accept rate for 3 parameters
  acclambda=0
  accbete1=0
  accbete0=0
  ## starting values for Markov chain
  mchain[1,] = c(stalambda,stabete1,stabete0)
  for (i in 2:Realization)
  {
    ## most upto date state for each parameter
    currlambda = mchain[i-1,1]
    currbete1 = mchain[i-1,2]
    currbete0 = mchain[i-1,3]
    ######################################################################################################    
    ## sample from full conditional distribution of lambda (Metropolis-Hastings update)
    # first, we create a function to calculate loglambda(pi):
    loglambdapi<- function(lambda, bete1,bete0)
    {return (dexpgauss(dat3[,2], bete0+bete1*(dat3[,1]),sigma, lambda, log = TRUE)-0.99*log(lambda)-lambda/100) }
    # next,draw one sample at random from uniform distribution
    lambdastar <- runif(1,0,1)
    ## Metropolis accept-reject step (in log scale)
    logMHratio1 = loglambdapi(lambdastar,currbete1,currbete0)-loglambdapi(currlambda,currbete1,currbete0)
    logalpha1 = min(0,logMHratio1) # alpha = min(1,MHratio)
    # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    if (log(runif(1))<logalpha1) 
    {
      acclambda = acclambda + 1 # increment count of accepted proposals
      currlambda = lambdastar
    }
    
    #########################################################################################################    
    ## sample from full conditional distribution of bete1 (Metropolis update)
    # First, we create a function to calculate logbete1(pi):
    logbete1pi<- function(lambda, bete1,bete0)
    {return (dexpgauss(dat3[,2], bete0+bete1*(dat3[,1]),sigma, lambda, log = TRUE)-bete1*bete1/200) }
    # draw one sample at random from normal distibution
    bete1star <- rnorm(1, mean=currbete1, sd=tau1)
    ## Metropolis accept-reject step (in log scale)
    logMHratio2 = logbete1pi(currlambda,bete1star,currbete0)-logbete1pi(currlambda,currbete1,currbete0)
    logalpha2 = min(0,logMHratio2) # alpha = min(1,MHratio)
    # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    if (log(runif(1))<logalpha2) 
    {
      accbete1 = accbete1 + 1 # increment count of accepted proposals
      currbete1 = bete1star
    }
    
    
    ########################################################################################################   
    ## sample from full conditional distribution of bete0 (Metropolis update)
    # First, we create a function to calculate logbete1(pi):
    logbete0pi<- function(lambda, bete1,bete0)
    {return (dexpgauss(dat3[,2], bete0+bete1*(dat3[,1]),sigma, lambda, log = TRUE)-bete0*bete0/200) }
    # draw one sample at random from normal distibution
    bete0star <- rnorm(1, mean=currbete0, sd=tau1)
    ## Metropolis accept-reject step (in log scale)
    logMHratio3 = logbete0pi(currlambda,currbete1,bete0star)-logbete0pi(currlambda,currbete1,currbete0)
    logalpha3 = min(0,logMHratio3) # alpha = min(1,MHratio)
    # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    if (log(runif(1))<logalpha3) 
    {
      accbete0 = accbete0 + 1 # increment count of accepted proposals
      currbete0 = bete0star
    }
    ##########################################################################################
    ## update chain with new values
    mchain[i,] = c(currlambda,currbete1,currbete0)
  }
  return(mchain)
}
a=posteriorsampler(Realization)
######################################################################################
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
  
  return(list (est=muhat,se=bmse,Q=quantile(vals,c(0.025,0.975))))
}
cat("##############################################################################\n
    calculate the estimation and MCMCse and 95% quantile for lambda")
bm(a[,1])
cat("##############################################################################\n
    calculate the estimation and MCMCse and 95% quantile for bete1")
bm(a[,2])
cat("##############################################################################\n
    calculate the estimation and MCMCse and 95% quantile for bete0")
bm(a[,3])
#############################################################################################
#get the autocorrelation of each estimation for 100000 realizations
acf(a[,1])
plot(density(a[,1]))
acf(a[,2])
plot(density(a[,2]))
acf(a[,3])
plot(density(a[,3]))













