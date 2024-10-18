### Call funtions from the websites #####

source('http://sites.stat.psu.edu/~mharan/batchmeans.R')
source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R')

### Read data ############################

dat1 <- read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat')
xobs1 <- dat1[,1]
yobs1 <- dat1[,2]

################################################
### No. 1 ######################################
################################################

### log.potr1() is the log-scale posterior density ######

log.potr1 <- function(b0, b1, sig, lam, xobs, yobs){
  sum(dexpgauss(x=yobs, mu=b0+b1*xobs, sigma=sig, lambda=lam, log=T)) - b1^2/200
}

### mh.emg1() is a M-H sampler for Problem 1 with a normal proposal #####
### n = sample size; x0 = initial value
### tau = sd for the normal proposal
### y = observed Y; x = observed X
### retrun sample and acceptance rate

mh.emg1 <- function(n, x0=0, tau=1, y, x){
  
  ## flag records the times of transitions, add 1 to flag when accepting the new state
  ## the inital value of flag is 1 b/c the initial state is always accepted  
  flag <- 1 
  samp <- rep(NA, n) ## record the sample
  samp[1] <- x0 ## x0 is the inital value
  xb1 <- x0 ## xb1 is the current state
  
  for(i in 2:n){
    yb1 <- rnorm(1, mean=xb1, sd=tau) ## yb1 is the state generated from the normal proposal
    
    ## p is the probability that accept the new state
    p <- min(1, exp(log.potr1(b0=5, b1=yb1, sig=1, lam=0.4, xobs=x, yobs=y) 
                    - log.potr1(b0=5, b1=xb1, sig=1, lam=0.4, xobs=x, yobs=y)))
    
    ## accept the new state yb1 with prob p, otherwise stay in current state xb1
    samp[i] <- sample(c(yb1, xb1), 1, prob=c(p, 1-p))
    xb1 <-samp[i]
    if (samp[i] == yb1) {flag <- flag + 1}
  } 
  
  rate <- flag / n ## acceptance rate
  mh <- list(samp, rate) ## retrun a list of sample and acceptance rate
  names(mh) <- c('samp', 'rate')
  return(mh)
}

#### 1(b) & (c) ############################

### different sample sizes
N <- c(1000, 5000, 10000, 20000, 50000, 100000) 

### sample by M-H sampler ####
mhsamp <- mh.emg1(max(N), x0=7, tau=0.9, y=yobs1, x=xobs1)

### acceptance rate
mhsamp$rate

### Calculate posterior mean, MCMCse, 95% CI and ess for beta1 
### with different sample sizes, results are stored in matrix 'result'

result <- matrix(NA, nrow=6, ncol=6)
for(i in 1:length(N)){
  result[i,1] <- N[i]   ## sample size
  result[i,2] <- mean(mhsamp$samp[1:N[i]])  ## posterior mean
  result[i,3] <- imse(mhsamp$samp[1:N[i]])  ## MCMCse
  q <- quantile(mhsamp$samp[1:N[i]], c(0.025, 0.975))  ## 95% CI
  result[i,4] <- q[1]
  result[i,5] <- q[2]
  result[i,6] <- ess(mhsamp$samp[1:N[i]])  ## ess
}
colnames(result) <- c('Sample size', 'Expectation', 'MCMCse', 'lower', 'upper','ess')
result

### 1(d) ############################

### Estimated density of beta1 #####
plot(density(mhsamp$samp), col=3, lwd=2, ylab='Estimated denstiy of beta1', main=' ')

### 1(e) ############################

M <- 100000
m <- 100
samsize <- seq(m, M, m)

### generate 3 samples with different starting values 

samp1 <- mh.emg1(M, x0=7, tau=1, y=yobs1, x=xobs1)$samp
samp2 <- mh.emg1(M, x0=0, tau=1, y=yobs1, x=xobs1)$samp
samp3 <- mh.emg1(M, x0=14, tau=1, y=yobs1, x=xobs1)$samp
samp <- rbind(samp1, samp2, samp3)

### mcse is a vecor to store MCMCse from the first sample

mcse = rep(NA, length(samsize))
for(i in 1:length(samsize)){
  mcse[i] <- imse(samp1[1:samsize[i]])
}


### plots of MCMCse, posterior mean, acf and estimated density

par(mfrow=c(2,2))

plot(samsize, mcse, type='l', xlab='Sample Size', ylab='MCMC standard error') ## plot of MCMCse


## plot of posterior mean

plot(0,0,ylim=c(7,7.5), xlim=range(samsize), xlab='Sample Size', ylab='Point estimate', 
     main= ' ')
for(j in 1:3){
  b1hat <- rep(NA, length(samsize))
  for(i in 1:length(samsize)){
    b1hat[i] <- mean(samp[j, 1:samsize[i]])
  }
  points(samsize, b1hat, type='l')
}  

acf(samp1, main=' ') ## acf plot

plot(density(samp1), col=3, lwd=2, ylab='Estimated denstiy of beta1', main=' ')
points(density(samp1[1:M/2]), col=4, lty=2, type='l')  ## plot of density

par(mfrow=c(1,1))