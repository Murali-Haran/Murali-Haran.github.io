### Call funtions from the websites #####

source('http://sites.stat.psu.edu/~mharan/batchmeans.R')
source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R')

### Read data ############################

dat3 <- read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat')
xobs3 <- dat3[,1]
yobs3 <- dat3[,2]

##################################################
### No. 3 ########################################
##################################################

### log.pstr2() is the log-scale posterior density ######

log.pstr2 <- function(b0, b1, sig, lam, yobs, xobs){
  result <- sum(dexpgauss(x=yobs, mu=b0+b1*xobs, sigma=sig, lambda=lam, log=T)) - b1^2/200 - b0^2/200 
  + dgamma(lam, shape=0.01, rate=0.01, log=T)
  return(result)
}


### mh.emg2() is a M-H sampler for Problem 2 #####
### n = sample size; (b0, b1, lam) = initial values for (beta0, beta1, lambda)
### taub0 = sd of the normal proposal for beta0
### taub1 = sd of the normal proposal for beta1
### taulam = sd of the truncated normal proposal for lambda
### y = observed Y; x = observed X
### retrun samples for beta0, beta1, lambda and acceptance rate

mh.emg2 <- function(n, b0, b1, lam, taub0, taub1, taulam, y, x){
  
  sampb0 <- rep(NA, n)
  sampb1 <- rep(NA, n)
  samplam <- rep(NA, n) ## store the sample for each parameter
  
  sampb0[1] <- b0
  sampb1[1] <- b1
  samplam[1] <- lam ## initial values
  
  xb0 <- b0
  xb1 <- b1
  xlam <- lam ### xb0, xb1 and xlam are the current states
  
  ## flagb0, flagb1 and flaglam record the times of transitions, 
  ## add 1 to flag when accepting the new state
  ## the inital value of flag is 1 b/c the initial state is always accepted  
  
  flagb0 <- flagb1 <- flaglam <- 1 
  
  for( i in 2:n){
    
    ## generate a new state for beta0
    yb0 <- rnorm(1, xb0, taub0)
    
    ## compute the probability
    pb0 <- min(1, exp( log.pstr2(yb0, xb1, 1, xlam, yobs=y, xobs=x)   - 
                         log.pstr2(xb0, xb1, 1, xlam, yobs=y, xobs=x)))
    
    ## accept the new state yb1 with prob pb0, otherwise stay in current state xb0
    sampb0[i] <- sample(c(yb0, xb0), 1, prob=c(pb0, 1-pb0))
    if (sampb0[i] == yb0) {flagb0 <- flagb0 + 1}
    xb0 <- sampb0[i]
    
    ## generate a new state for beta1
    yb1 <- rnorm(1, xb1, taub1)
    
    ## compute the probability
    pb1 <- min(1, exp( log.pstr2(xb0, yb1, 1, xlam, yobs=y, xobs=x)   - 
                         log.pstr2(xb0, xb1, 1, xlam, yobs=y, xobs=x)))
    
    ## accept the new state yb1 with prob pb1, otherwise stay in current state xb1
    sampb1[i] <- sample(c(yb1, xb1), 1, prob=c(pb1, 1-pb1))
    if (sampb1[i] == yb1) {flagb1 <- flagb1 + 1}
    xb1 <- sampb1[i]
    
    ## generate a new state for lambda
    ylam <- rnorm(1, xlam, taulam)
    while(ylam <= 0) {ylam <- rnorm(1, xlam, taulam)}
    
    ## compute the probability
    cy <- 1 - pnorm(0, ylam, taulam) 
    cx <- 1 - pnorm(0, xlam, taulam) ## nomalizing constant
    plam <- min(1, exp( log.pstr2(xb0, xb1, 1, ylam, yobs=y, xobs=x) + dnorm(xlam, ylam, taulam,log=T)
                        -log(cy)
                        - log.pstr2(xb0, xb1, 1, xlam, yobs=y, xobs=x) - dnorm(ylam, xlam, taulam,log=T)
                        + log(cx)
    ))
    
    ## accept the new state yb1 with prob plam, otherwise stay in current state xlam
    samplam[i] <- sample(c(ylam, xlam), 1, prob=c(plam, 1-plam))
    if (samplam[i] == ylam) {flaglam <- flaglam + 1}
    xlam <- samplam[i]
  }
  
  rate <- c(flagb0/n, flagb1/n, flaglam/n) ## acceptance rate
  names(rate) <- c('beta0', 'beta1', 'lambda')
  emg <- list(sampb0, sampb1, samplam, rate) ## a list of samples and acceptance rate
  names(emg) <- c('beta0', 'beta1', 'lambda', 'rate')
  return(emg)
}

### 3(a)

### Calculate posterior mean, MCMCse, 95% CI and ess for each parameter (sample size = 200000)
### results are stored in matrix 'result3'

sam3 <- mh.emg2(n=200000,b0=0.15,b1=2.45,lam=0.16,taub0=0.3,taub1=0.5,taulam=0.02, y=yobs3, x=xobs3)
sam3b0 <- sam3$beta0
sam3b1 <- sam3$beta1
sam3lam <- sam3$lambda

result3 <- matrix(NA, ncol=6, nrow=3)
colnames(result3) <- c('expectation', 'MCMCse', 'lower', 'upper', 'Acpt.rate','ESS')
result3[1,] <- c(unlist(bm(sam3b0)), quantile(sam3b0, c(0.025, 0.975)), sam3$rate[1], ess(sam3b0))
result3[2,] <- c(unlist(bm(sam3b1)), quantile(sam3b1, c(0.025, 0.975)), sam3$rate[2], ess(sam3b1))
result3[3,] <- c(unlist(bm(sam3lam)), quantile(sam3lam, c(0.025, 0.975)), sam3$rate[3], ess(sam3lam))
result3


### 3(b)

### estimates density for marginal distributions

par(mfrow=c(1,3))
plot(density(sam3b0), col=3, lwd=2, ylab='Estimated denstiy of beta0', main=' ')
plot(density(sam3b1), col=3, lwd=2, ylab='Estimated denstiy of beta1', main=' ')
plot(density(sam3lam), col=3, lwd=2, ylab='Estimated denstiy of lambda', main=' ')
par(mfrow=c(1,1))