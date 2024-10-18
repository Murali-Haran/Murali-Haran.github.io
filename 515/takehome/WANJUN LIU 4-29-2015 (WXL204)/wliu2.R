### Call funtions from the websites #####

source('http://sites.stat.psu.edu/~mharan/batchmeans.R')
source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R')

### Read data ############################

dat2 <- read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat')
xobs2 <- dat2[,1]
yobs2 <- dat2[,2]

##################################################
### No. 2 ########################################
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

### 2(b) 

### posterior mean, MCMCse, 95% CI ####

## generate samples with size = 200000

s2 <- mh.emg2(n=200000,b0=2.35,b1=3.45,lam=0.8,taub=0.2,taub1=0.3,taulam=0.1, y=yobs2, x=xobs2)
s2b0 <- s2$beta0 ## sample for beta0
s2b1 <- s2$beta1 ## sample for beta1
s2lam <- s2$lambda ## sample for lambda


### Calculate posterior mean, MCMCse, 95% CI and ess for each parameter 
### results are stored in matrix 'result2'

result2 <- matrix(NA, ncol=6, nrow=3)
colnames(result2) <- c('expectation', 'MCMCse', 'lower', 'upper', 'Acpt.rate', 'ESS')
result2[1,] <- c(unlist(bm(s2b0)), quantile(s2b0, c(0.025, 0.975)), s2$rate[1], ess(s2b0))
result2[2,] <- c(unlist(bm(s2b1)), quantile(s2b1, c(0.025, 0.975)), s2$rate[2], ess(s2b1))
result2[3,] <- c(unlist(bm(s2lam)), quantile(s2lam, c(0.025, 0.975)), s2$rate[3], ess(s2lam))
result2

### 2(c)

### estimate correlation between beta0 and beta1

cor(s2b0, s2b1)

### 2(d)

### estimates density for marginal distributions

par(mfrow=c(1,3))
plot(density(s2b0), col=3, lwd=2, ylab='Estimated denstiy of beta0', main=' ')
plot(density(s2b1), col=3, lwd=2, ylab='Estimated denstiy of beta1', main=' ')
plot(density(s2lam), col=3, lwd=2, ylab='Estimated denstiy of lambda', main=' ')
par(mfrow=c(1,1))

### 2(e)

## generate another two samples with size = 200000, but with different starting values
## in part 2(b), we have generated a sample called 's2'

s3 <- mh.emg2(n=200000,b0=-4,b1=-5,lam=8,taub=0.2,taub1=0.3,taulam=0.1, y=yobs2, x=xobs2)
s3b0 <- s3$beta0
s3b1 <- s3$beta1
s3lam <- s3$lambda

s4 <- mh.emg2(n=200000,b0=8,b1=10,lam=4,taub=0.2,taub1=0.3,taulam=0.1, y=yobs2, x=xobs2)
s4b0 <- s4$beta0
s4b1 <- s4$beta1
s4lam <- s4$lambda

msize <- 200000
m <- 100
samsize <- seq(m, msize, by=m)
mmcse <- matrix(NA, nrow=3, ncol=length(samsize)) ## store MCMCse from sample 's2'
msam1 <- matrix(NA, nrow=3, ncol=length(samsize)) ## store posterior mean from sample 's2'
msam2 <- matrix(NA, nrow=3, ncol=length(samsize)) ## store posterior mean from sample 's3'
msam3 <- matrix(NA, nrow=3, ncol=length(samsize)) ## store posterior mean from sample 's4'

### use bm() to calculate estimation and MCMCse from sample 's2'

for(i in 1:length(samsize)){
  mmcse[1,i] <- bm(s2b0[1:samsize[i]])$se
  mmcse[2,i] <- bm(s2b1[1:samsize[i]])$se
  mmcse[3,i] <- bm(s2lam[1:samsize[i]])$se
  msam1[1,i] <- bm(s2b0[1:samsize[i]])$est
  msam1[2,i] <- bm(s2b1[1:samsize[i]])$est
  msam1[3,i] <- bm(s2lam[1:samsize[i]])$est
}

### use bm() to calculate estimation from sample 's3'

for(i in 1:length(samsize)){
  msam2[1,i] <- bm(s3b0[1:samsize[i]])$est
  msam2[2,i] <- bm(s3b1[1:samsize[i]])$est
  msam2[3,i] <- bm(s3lam[1:samsize[i]])$est
}

### use bm() to calculate estimation from sample 's4'

for(i in 1:length(samsize)){
  msam3[1,i] <- bm(s4b0[1:samsize[i]])$est
  msam3[2,i] <- bm(s4b1[1:samsize[i]])$est
  msam3[3,i] <- bm(s4lam[1:samsize[i]])$est
}


par(mfrow=c(3,3))

### plots of MCMCse, posterior mean and acf for beta0

plot(samsize, mmcse[1,], type='l', xlab='sample size', ylab='MCse (beta0)')
plot(samsize, rep(0, length(samsize)), ylim=c(2,3), type='l', xlab='sample size', ylab='mean (beta0)')
points(samsize, msam1[1,], type='l')
points(samsize, msam2[1,], type='l')
points(samsize, msam3[1,], type='l')
acf(s2b0, main='beta0')

### plots of MCMCse, posterior mean and acf for beta1

plot(samsize, mmcse[2,], type='l', xlab='sample size', ylab='MCse (beta1)')
plot(samsize, rep(0, length(samsize)), ylim=c(3,4), type='l', xlab='sample size', ylab='mean (beta1)')
points(samsize, msam1[2,], type='l')
points(samsize, msam2[2,], type='l')
points(samsize, msam3[2,], type='l')
acf(s2b1, main='beta1')

### plots of MCMCse, posterior mean and acf for lambda

plot(samsize, mmcse[3,], type='l', xlab='sample size', ylab='MCse (lambda)')
plot(samsize, rep(0, length(samsize)), ylim=c(0.7,1.5), type='l', xlab='sample size', ylab='mean (lambda)')
points(samsize, msam1[3,], type='l')
points(samsize, msam2[3,], type='l')
points(samsize, msam3[3,], type='l')
acf(s2lam, main='lambda')

par(mfrow=c(1,1))
