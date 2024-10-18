### get functions

## MCMC sampler for Problem 1
MHsamp1 <- function(numIt,dat,b1in,b1SD) {
  
  # set up vector to store Markov chain values
  mchain <- rep(NA,numIt)
  
  # count number of accepted proposals
  acc <- 0
  
  # starting value for Markov chain
  mchain[1] <- b1in
  
  # main algorithm
  for (i in 2:numIt) {
    
    # get most updated state
    currb1 <- mchain[i-1]
    
    # sample from random walk Normal proposal
    propb1 <- rnorm(1,currb1,b1SD)
    
    # compute Metropolis ratio (in log scale)
    logMHratio <- sum(dexpgauss(dat[,2],5+propb1*dat[,1],1,.4,log=TRUE))
    logMHratio <- logMHratio+dnorm(propb1,0,10,log=TRUE)
    logMHratio <- logMHratio-sum(dexpgauss(dat[,2],5+currb1*dat[,1],1,.4,log=TRUE))
    logMHratio <- logMHratio-dnorm(currb1,0,10,log=TRUE)
    
    # MH accept-reject step
    logalpha <- min(0,logMHratio)
    if (log(runif(1))<logalpha) {
      acc <- acc + 1
      currb1 <- propb1
    }
    
    # update Markov chain with new value
    mchain[i] <- currb1
    
  } # end main algorithm
  
  # print statements
  cat("Markov chain algorithm ran for ",numIt,"iterations\n")
  cat("Acceptance rate was ",acc/(numIt-1),"\n")
  
  # return Markov chain
  return(mchain)
}

########################################

## MCMC sampler for Problems 2 and 3
MHsamp2 <- function(numIt,dat,b0in,b0SD,b1in,b1SD,corb,lbin,lbscale) {  
  
  # set up matrix to store Markov chain values
  # each row corresponds to one of 3 parameters in order: beta1,beta2,lambda
  # each column corresponds to a single state of the Markov chain
  mchain <- matrix(NA,3,numIt)
  
  # count number of accepted proposals
  accbeta <- 0
  acclb <- 0
  
  # starting value for Markov chain
  mchain[,1] <- c(b0in,b1in,lbin)
  
  # main algorithm
  for (i in 2:numIt) {
    
    # get most updated state
    currb0 <- mchain[1,i-1]
    currb1 <- mchain[2,i-1]
    currlb <- mchain[3,i-1]
    
    # update beta
    Covb <- corb*b0SD*b1SD
    Sigmab <- matrix(c(b0SD^2,Covb,Covb,b1SD^2),2,2)
    beta <- mvrnorm(1,c(currb0,currb1),Sigmab)
    propb0 <- beta[1]
    propb1 <- beta[2]    
    lnMHrat <- sum(dexpgauss(dat[,2],propb0+propb1*dat[,1],1,currlb,log=TRUE))
    lnMHrat <- lnMHrat+dnorm(propb0,0,10,log=TRUE)+dnorm(propb1,0,10,log=TRUE)
    lnMHrat <- lnMHrat-sum(dexpgauss(dat[,2],currb0+currb1*dat[,1],1,currlb,log=TRUE))
    lnMHrat <- lnMHrat-dnorm(currb0,0,10,log=TRUE)-dnorm(currb1,0,10,log=TRUE)
    logalphabeta <- min(0,lnMHrat)
    if (log(runif(1))<logalphabeta) {
      accbeta <- accbeta + 1
      currb0 <- propb0
      currb1 <- propb1
    }       
    
    # update lambda
    proplb <- rgamma(1,shape=currlb/lbscale,scale=lbscale)
    lnMHrat <- sum(dexpgauss(dat[,2],currb0+currb1*dat[,1],1,proplb,log=TRUE))
    lnMHrat <- lnMHrat+dgamma(proplb,shape=.01,scale=100,log=TRUE)
    lnMHrat <- lnMHrat-sum(dexpgauss(dat[,2],currb0+currb1*dat[,1],1,currlb,log=TRUE))
    lnMHrat <- lnMHrat-dgamma(currlb,shape=.01,scale=100,log=TRUE)
    lnMHrat <- lnMHrat-dgamma(proplb,shape=currlb/lbscale,scale=lbscale)
    lnMHrat <- lnMHrat+dgamma(currlb,shape=proplb/lbscale,scale=lbscale)
    logalphalb <- min(0,lnMHrat)
    if (log(runif(1))<logalphalb) {
      acclb <- acclb + 1
      currlb <- proplb
    }  
    
    # update Markov chain with new values
    mchain[,i] <- c(currb0,currb1,currlb)
    
  } # end main algorithm
  
  # print statements
  cat("Markov chain algorithm ran for ",numIt,"iterations\n")
  cat("Acceptance rate for beta=",accbeta/(numIt-1),"\n")
  cat("Acceptance rate for lambda=",acclb/(numIt-1),"\n")
  
  # return Markov chain
  return(mchain)
}

########################################

## call other functions

# library for multivariate normal
install.packages("MASS")
require(MASS)

# source code for EMG density function
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")

# source code to compute MCMC standard error
source("http://www.stat.psu.edu/~mharan/batchmeans.R")

###############################################################
###############################################################

### Problem 1

## inputs

# read in dataset
EMG1 <- read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
head(EMG1)
plot(EMG1)

# number of samples desired
Nsamp1 <- 24000

# initial value of beta1
beta1in1 <- 7.3

# standard deviation of proposal for beta1
beta1SD1 <- .9

########################################

## report final values

# run algorithm
MCMCsamp1 <- MHsamp1(Nsamp1,EMG1,beta1in1,beta1SD1)

# report estimate
mean(MCMCsamp1)

# report standard error
bm(MCMCsamp1)$se

# report a 95% CI
quantile(MCMCsamp1,c(.025,.975))

# plot smoothed density of samples
plot(density(MCMCsamp1),main="Problem 1 - Beta 1")

# check acf
acf(MCMCsamp1,main="Problem 1 - Beta 1")

# compute effective sample size
ess(MCMCsamp1)

########################################

## diagnostics

# sequence of sample sizes
seqSamp1 <- seq(2000,Nsamp1,by=2000)

# vectors for estimates and standard errors
est1 <- rep(NA,length(seqSamp1))
SE1 <- rep(NA,length(seqSamp1))
estU1 <- rep(NA,length(seqSamp1))
estD1 <- rep(NA,length(seqSamp1))

# loop over sample sizes
for (i in 1:length(seqSamp1)) {
  MCMCdiag1 <- MHsamp1(seqSamp1[i],EMG1,beta1in1,beta1SD1)
  est1[i] <- mean(MCMCdiag1)
  SE1[i] <- bm(MCMCdiag1)$se
  estU1[i] <- mean(MHsamp1(seqSamp1[i],EMG1,10,beta1SD1))
  estD1[i] <- mean(MHsamp1(seqSamp1[i],EMG1,5,beta1SD1))
  if (seqSamp1[i]==Nsamp1/2) {sampMiddle1 <- MCMCdiag1}
}

# estimates
plot(seqSamp1,est1,type="l",ylim=c(7.3,7.38),
     xlab="Sample Size",ylab="Estimate",main="Problem 1 - Beta 1")
lines(seqSamp1,estU1,type="l",lty=2)
lines(seqSamp1,estD1,type="l",lty=3)

# standard errors
plot(seqSamp1,SE1,type="l",xlab="Sample Size",main="Problem 1 - Beta 1")

# plot density with half sample size and full sample size
plot(density(MCMCdiag1),ylim=c(0,1.5),main="Problem 1 - Beta 1")
lines(density(sampMiddle1),lty=2)
legend(6,1.4,c(paste("N=",Nsamp1),paste("N=",Nsamp1/2)),lty=c(1,2))

###############################################################
###############################################################

### Problem 2

## inputs

# read in dataset
EMG2 <- read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
head(EMG2)
plot(EMG2)

# number of samples desired
Nsamp2 <- 70000

# initial values
b0in2 <- 2.4
b1in2 <- 3.5
lbin2 <- .8

# scale parameters for proposals
b0SD2 <- .2
b1SD2 <- .25
lbscale2 <- .02

# correlation between beta0 and beta1
corrb2 <- -.8

########################################

## report final values

# run algorithm
MCMCsamp2 <- MHsamp2(Nsamp2,EMG2,b0in2,b0SD2,b1in2,b1SD2,corrb2,lbin2,lbscale2)

# report estimates
mean(MCMCsamp2[1,])
bm(MCMCsamp2[1,])$se
quantile(MCMCsamp2[1,],c(.025,.975))
mean(MCMCsamp2[2,])
bm(MCMCsamp2[2,])$se
quantile(MCMCsamp2[2,],c(.025,.975))
mean(MCMCsamp2[3,])
bm(MCMCsamp2[3,])$se
quantile(MCMCsamp2[3,],c(.025,.975))

# report correlation between beta0 and beta1
cor(MCMCsamp2[1,],MCMCsamp2[2,])

# plot smoothed density of samples
plot(density(MCMCsamp2[1,]),main="Problem 2 - Beta 0")
plot(density(MCMCsamp2[2,]),main="Problem 2 - Beta 1")
plot(density(MCMCsamp2[3,]),main="Problem 2 - Lambda")

# check acf
acf(MCMCsamp2[1,],main="Problem 2 - Beta 0")
acf(MCMCsamp2[2,],main="Problem 2 - Beta 1")
acf(MCMCsamp2[3,],main="Problem 2 - Lambda")

# compute effective sample size
ess(MCMCsamp2[1,])
ess(MCMCsamp2[2,])
ess(MCMCsamp2[3,])

########################################

## diagnostics

# sequence of sample sizes
seqSamp2 <- seq(5000,Nsamp2,by=5000)

# matrices for estimates and standard errors
est2 <- matrix(NA,3,length(seqSamp2))
SE2 <- matrix(NA,3,length(seqSamp2))
estU2 <- matrix(NA,3,length(seqSamp2))
estD2 <- matrix(NA,3,length(seqSamp2))

# loop over sample sizes
for (j in 1:length(seqSamp2)) {
  MCMCdiag2 <- MHsamp2(seqSamp2[j],EMG2,b0in2,b0SD2,b1in2,b1SD2,corrb2,lbin2,lbscale2)
  MCMCdiagU2 <- MHsamp2(seqSamp2[j],EMG2,5,b0SD2,6,b1SD2,corrb2,1.3,lbscale2)
  MCMCdiagD2 <- MHsamp2(seqSamp2[j],EMG2,0,b0SD2,1,b1SD2,corrb2,.3,lbscale2)
  if (seqSamp2[j]==Nsamp2/2) {sampMiddle2 <- MCMCdiag2}
  for (i in 1:3) {
    est2[i,j] <- mean(MCMCdiag2[i,])
    estU2[i,j] <- mean(MCMCdiagU2[i,])
    estD2[i,j] <- mean(MCMCdiagD2[i,])
    SE2[i,j] <- bm(MCMCdiag2[i,])$se
  }
}

# beta 0
plot(seqSamp2,est2[1,],type="l",ylim=c(2.3,2.4),
     xlab="Sample Size",ylab="Estimate",main="Problem 2 - Beta 0")
lines(seqSamp2,estU2[1,],type="l",lty=2)
lines(seqSamp2,estD2[1,],type="l",lty=3)
plot(seqSamp2,SE2[1,],type="l",xlab="Sample Size",ylab="SE",main="Problem 2 - Beta 0")
plot(density(MCMCdiag2[1,]),ylim=c(0,3.5),main="Problem 2 - Beta 0")
lines(density(sampMiddle2[1,]),lty=2)
legend(2.55,3.25,c(paste("N=",Nsamp2),paste("N=",Nsamp2/2)),lty=c(1,2))

# beta 1
plot(seqSamp2,est2[2,],type="l",ylim=c(3.4,3.5),
     xlab="Sample Size",ylab="Estimate",main="Problem 2 - Beta 1")
lines(seqSamp2,estU2[2,],type="l",lty=2)
lines(seqSamp2,estD2[2,],type="l",lty=3)
plot(seqSamp2,SE2[2,],type="l",xlab="Sample Size",ylab="SE",main="Problem 2 - Beta 1")
plot(density(MCMCdiag2[2,]),ylim=c(0,2.5),main="Problem 2 - Beta 1")
lines(density(sampMiddle2[2,]),lty=2)
legend(3.75,2.25,c(paste("N=",Nsamp2),paste("N=",Nsamp2/2)),lty=c(1,2))

# lambda
plot(seqSamp2,est2[3,],type="l",ylim=c(.78,.92),
     xlab="Sample Size",ylab="Estimate",main="Problem 2 - Lambda")
lines(seqSamp2,estU2[3,],type="l",lty=2)
lines(seqSamp2,estD2[3,],type="l",lty=3)
plot(seqSamp2,SE2[3,],type="l",xlab="Sample Size",ylab="SE",main="Problem 2 - Lambda")
plot(density(MCMCdiag2[3,]),ylim=c(0,8),main="Problem 2 - Lambda")
lines(density(sampMiddle2[3,]),lty=2)
legend(.925,7,c(paste("N=",Nsamp2),paste("N=",Nsamp2/2)),lty=c(1,2))

###############################################################
###############################################################

### Problem 3

## inputs

# read in dataset
EMG3 <- read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")
head(EMG3)
plot(EMG3)

# number of samples desired
Nsamp3 <- 48000

# initial values
b0in3 <- .15
b1in3 <- 2.5
lbin3 <- .16

# scale parameters for proposals
b0SD3 <- .22
b1SD3 <- .38
lbscale3 <- .001

# correlation between beta0 and beta1
corrb3 <- -.8

########################################

## report final values

# run algorithm
MCMCsamp3 <- MHsamp2(Nsamp3,EMG3,b0in3,b0SD3,b1in3,b1SD3,corrb3,lbin3,lbscale3)

# report estimates
mean(MCMCsamp3[1,])
bm(MCMCsamp3[1,])$se
quantile(MCMCsamp3[1,],c(.025,.975))
mean(MCMCsamp3[2,])
bm(MCMCsamp3[2,])$se
quantile(MCMCsamp3[2,],c(.025,.975))
mean(MCMCsamp3[3,])
bm(MCMCsamp3[3,])$se
quantile(MCMCsamp3[3,],c(.025,.975))

# report correlation between beta0 and beta1
cor(MCMCsamp3[1,],MCMCsamp3[2,])

# plot smoothed density of samples
plot(density(MCMCsamp3[1,]),main="Problem 3 - Beta 0")
plot(density(MCMCsamp3[2,]),main="Problem 3 - Beta 1")
plot(density(MCMCsamp3[3,]),main="Problem 3 - Lambda")

# check acf
acf(MCMCsamp3[1,],main="Problem 3 - Beta 0")
acf(MCMCsamp3[2,],main="Problem 3 - Beta 1")
acf(MCMCsamp3[3,],main="Problem 3 - Lambda")

# compute effective sample size
ess(MCMCsamp3[1,])
ess(MCMCsamp3[2,])
ess(MCMCsamp3[3,])

########################################

## diagnostics

# sequence of sample sizes
seqSamp3 <- seq(4000,Nsamp3,by=4000)

# matrices for estimates and standard errors
est3 <- matrix(NA,3,length(seqSamp3))
SE3 <- matrix(NA,3,length(seqSamp3))
estU3 <- matrix(NA,3,length(seqSamp3))
estD3 <- matrix(NA,3,length(seqSamp3))

# loop over sample sizes
for (j in 1:length(seqSamp3)) {
  MCMCdiag3 <- MHsamp2(seqSamp3[j],EMG3,b0in3,b0SD3,b1in3,b1SD3,corrb3,lbin3,lbscale3)
  MCMCdiagU3 <- MHsamp2(seqSamp3[j],EMG3,1,b0SD3,5,b1SD3,corrb3,.3,lbscale3)
  MCMCdiagD3 <- MHsamp2(seqSamp3[j],EMG3,-1,b0SD3,0,b1SD3,corrb3,.05,lbscale3)
  if (seqSamp3[j]==Nsamp3/2) {sampMiddle3 <- MCMCdiag3}
  for (i in 1:3) {
    est3[i,j] <- mean(MCMCdiag3[i,])
    estU3[i,j] <- mean(MCMCdiagU3[i,])
    estD3[i,j] <- mean(MCMCdiagD3[i,])
    SE3[i,j] <- bm(MCMCdiag3[i,])$se
  }
}

# beta 0
plot(seqSamp3,est3[1,],type="l",ylim=c(.16,.19),
     xlab="Sample Size",ylab="Estimate",main="Problem 3 - Beta 0")
lines(seqSamp3,estU3[1,],type="l",lty=2)
lines(seqSamp3,estD3[1,],type="l",lty=3)
plot(seqSamp3,SE3[1,],type="l",xlab="Sample Size",ylab="SE",main="Problem 3 - Beta 0")
plot(density(MCMCdiag3[1,]),ylim=c(0,3),main="Problem 3 - Beta 0")
lines(density(sampMiddle3[1,]),lty=2)
legend(-.5,2.8,c(paste("N=",Nsamp3),paste("N=",Nsamp3/2)),lty=c(1,2))

# beta 1
plot(seqSamp3,est3[2,],type="l",ylim=c(2.45,2.5),
     xlab="Sample Size",ylab="Estimate",main="Problem 3 - Beta 1")
lines(seqSamp3,estU3[2,],type="l",lty=2)
lines(seqSamp3,estD3[2,],type="l",lty=3)
plot(seqSamp3,SE3[2,],type="l",xlab="Sample Size",ylab="SE",main="Problem 3 - Beta 1")
plot(density(MCMCdiag3[2,]),ylim=c(0,1.8),main="Problem 3 - Beta 1")
lines(density(sampMiddle3[2,]),lty=2)
legend(2.9,1.6,c(paste("N=",Nsamp3),paste("N=",Nsamp3/2)),lty=c(1,2))

# lambda
plot(seqSamp3,est3[3,],type="l",ylim=c(.1655,.1672),
     xlab="Sample Size",ylab="Estimate",main="Problem 3 - Lambda")
lines(seqSamp3,estU3[3,],type="l",lty=2)
lines(seqSamp3,estD3[3,],type="l",lty=3)
plot(seqSamp3,SE3[3,],type="l",xlab="Sample Size",ylab="SE",main="Problem 3 - Lambda")
plot(density(MCMCdiag3[3,]),ylim=c(0,80),main="Problem 3 - Lambda")
lines(density(sampMiddle3[3,]),lty=2)
legend(.142,70,c(paste("N=",Nsamp3),paste("N=",Nsamp3/2)),lty=c(1,2))
