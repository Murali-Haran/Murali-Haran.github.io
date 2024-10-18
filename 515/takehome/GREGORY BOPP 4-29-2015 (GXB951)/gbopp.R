### Read in MCMC functions
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")
source("http://sites.stat.psu.edu/~mharan/batchmeans.R")
library(MASS)

### Read in data
dat1<-read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
dat2<-read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
dat3<-read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")


###
### Function Definitions
###

########################################################################
### Function: seq.est
### Usage: Calculate Sequential estimates with increasing sample size
### Arguments:
# samp: vector of samples on which to calculate sequential estimates
# g: summarizing function to estimate over time with increasing sample size
# batchsize: increment to grow the sample size
# select.args: named elements to return if g returns a list
########################################################################
seq.est  = function(samp, g=mean, batchsize=100, select.args=NA)
{  
  est = c()
  # Default - use if g does not return a list by default
  if(is.na(select.args)){
    for (i in seq(from= batchsize,to = length(samp),by=batchsize))
    {
      est = c(est, g(samp[1:i]))
    }
  }
  # If select.args contains vector of strings of objects that g returns
  # return only those objects
  else{
    for (i in seq(batchsize,length(samp),by=batchsize))
    {
      est = c(est, g(samp[1:i])[[select.args]])
    }
  }
  return(est)
}

#######################################################################
### Function: logd.b1
### Usage: Calculates the log posterior density (up to normalizing constant)
#          of beta1 for question 1.
### Arguments:
# b1:   location at which to evaluate log posterior density
# b0:   fixed known value given in problem
# lam:  fixed known value given in problem
# sig:  fixed known value given in problem
########################################################################
logd.b1<-function(b1, b0=5, lam=0.4,sig=1){
  log.d<-sum(dexpgauss(y, mu = b0+b1*x, sigma = sig, 
                       lambda = lam, log = TRUE))-b1^2/200
  return(log.d)
}


#######################################################################
### Function: mhsampler1 (for question 1 only)
### Usage: MH Algorithm implementation for Q1 to generate samples of 
#          Beta1 from posterior distribution using a 
#          N(., tau^2) proposal.
#
### Arguments:
# init.b1:  Beta1 initialization (Starting value for Markov Chain)
# N:        Number of samples to generate
# tau:      sd of proposal distribution (tuning parameter)
########################################################################
mhsampler1<-function(init.b1,N=10000, tau=0.75){
  b1.smp<-rep(NA,N)   # Create empty vector to store samples
  b1.smp[1]<-init.b1  # Initial value for b1
  acpt<-1             # Track acceptance rate
  for(i in 2:N){
    b1.p<-rnorm(1,mean=b1.smp[i-1],sd=tau) # Propose new location
    if(log(runif(1))<logd.b1(b1.p)-logd.b1(b1.smp[i-1])){
      b1.smp[i]<-b1.p  # Update current beta_1 (if proposal is accepted)
      acpt<-acpt+1     # Increment acceptance count
    }
    else{
      b1.smp[i]<-b1.smp[i-1]    # Update current beta_1(if proposal is rejected)
    } 
  }
  a.rt<-acpt/N        # Calcuate acceptance rate
  print(paste("Acceptance Rate: ", a.rt))
  return(b1.smp)
}


#######################################################################
### Function: r.prop
### Usage: Random multivariate (3-d) normal generator for beta_0, beta_1, 
# lambda
#
### Arguments:
# in.param: current beta_0, beta_1, and phi locations of MH sampler
# tau0, tau1, tau2: tuning sd parameters for beta_0, beta_1, and phi 
#                   proposals.
########################################################################

r.prop<-function(in.param, tau0=0.5, tau1=0.5, tau2=0.5){
  # Calculate Covariance Matrix
  S<-rbind(c(tau0^2, -0.76*tau0*tau1, 0.42*tau0*tau2      ),
           c(-0.76*tau0*tau1,  tau1^2, 0.01*tau1*tau2    ),
           c(0.42*tau0*tau2, 0.01*tau1*tau2, tau2^2 ))
  # Generate proposal locations
  mvrnorm(n = 1,mu = in.param, Sigma=S)
}


#######################################################################
### Function: mhsampler2
### Usage: MH sampler for question 2. All-at-once MH sampler for beta_0
#          beta_1, and lambda
#
### Arguments:
# N: Number of samples to generate
# in.param: current beta_0, beta_1, and lambda (i.e. exp(phi)) locations 
#           of MH sampler
# tau.l:  list containing tuning sd parameters for beta_0, beta_1, and phi 
#          proposals. Elements of this list tb0, tb1, and tphi 
#          respectively
########################################################################
mhsampler2<-function(N, init.param, tau.l=list(tb0=, tb1=, tphi=)){
  # Initialize parameters  
  b0.cur<- init.param[1]
  b1.cur<- init.param[2]
  lam.cur<-init.param[3]
  
  acpt.cnt<-0                                # Counters for acceptance rate calculation
  param.mat<-matrix(ncol=3,nrow=N)           # store parameter MC chain in a matrix
  param.mat[1,]<-c(b0.cur,b1.cur,lam.cur)    # Initialize first state
  for(i in 2:N){
    # Propose New Location
    new.param<-r.prop(c(b0.cur, b1.cur, log(lam.cur)), tau0=tau.l$tb0, tau1=tau.l$tb1, tau2 = tau.l$tphi)
    # Evaluate calculate log of target kernels at old and new location
    n.log.d<-sum(dexpgauss(y, mu = new.param[1]+new.param[2]*x, sigma=1, 
                           lambda=exp(new.param[3]), log = TRUE))-new.param[1]^2/200-
                              new.param[2]^2/200+0.01*(new.param[3]-exp(new.param[3]))
    o.log.d<-sum(dexpgauss(y, mu = b0.cur+b1.cur*x, sigma=1, 
                           lambda=lam.cur, log = TRUE))-b0.cur^2/200-b1.cur^2/200+0.01*(exp(lam.cur)-exp(exp(lam.cur)))
    # Accept reject Step
    if(log(runif(1))<n.log.d-o.log.d){
      # Update Block if Accepted
      b0.cur<-new.param[1]             
      b1.cur<-new.param[2]
      lam.cur<-exp(new.param[3])
      # Store updates
      param.mat[i,1]<-b0.cur
      param.mat[i,2]<-b1.cur
      param.mat[i,3]<-lam.cur
      
      acpt.cnt<-acpt.cnt + 1
    }
    else{
      # Update Block if Rejected and store updates
      param.mat[i,1]<-b0.cur
      param.mat[i,2]<-b1.cur
      param.mat[i,3]<-lam.cur
    }     
  }
  # Calculate acceptance rates
  acpt.rts<-list(a.rt = acpt.cnt/N)
  # Return MC chain and acceptance rates
  return(list(param.mat= param.mat, acpt.rts=acpt.rts))
}


#######################################################################
### Functions: r.qb0b1,r.qphi 
### Usage: each generates a random variable or random vector from proposal 
#          conditional on current sample

### Arguments:
# b0b1: vector of (Beta_0, Beta_1) (current location of MC)
# phi: log(lambda) (current location of MC)
# tau: variance tuning parameter for a given proposal
########################################################################
r.qb0b1<-function(b0b1, tau0=0.5, tau1=0.5){
  S<-rbind(c(tau0^2,-0.8*tau0*tau1),c(-0.8*tau0*tau1,tau1^2)) # Calculate Covariance Matrix
  mvrnorm(n = 1, b0b1, Sigma=S)                               # Generate Proposal
}

r.qphi<-function(phi, tau=0.5){
  rnorm(1,mean= phi, sd = tau)    # Generate Proposal
}

########################################################################
### Function: mhsampler3
### Usage: MH Sampling Algorithm for question 3
#          a block MH updates (where blocks are (beta_0, beta_1), and lambda), 
#          generates samples from the posterior dist'n of beta_0, beta_1, and 
#          lambda. 
#
### Arguments:
# N: Number of samples to generate from the MC
# init.param: parameter initialization for MH algorithm 
#             pass a vector in the following order c(b0,b1,lam)
# tau.l:      list of tau tuning parameters for beta0, beta1, and phi 
#             proposal distributions (tb0, tb1, and tphi) respectively
########################################################################
mhsampler3<-function(N, init.param, tau.l=list(tb0=, tb1=, tphi=)){
  ## N: number of iterations to run MH sampler
  ## init.param: vector containing initial parameter values 
  ## tau.l: list containing tau parameter in proposal function (may be sd or df if using normal or t respectively)
  
  # Initialize parameters  
  b0.cur<- init.param[1]
  b1.cur<- init.param[2]
  lam.cur<-init.param[3]
  # Counters for acceptance rate calculation
  b1b2.acpt.cnt<- lam.acpt.cnt<-0
  
  param.mat<-matrix(ncol=3,nrow=N)           # store parameter MC chain in a matrix
  param.mat[1,]<-c(b0.cur,b1.cur,lam.cur)    # Initialize first state
  for(i in 2:N){
    # Update b0 and b1
    new.b0b1<-r.qb0b1(c(b0.cur, b1.cur), tau0=tau.l$tb0, tau1=tau.l$tb1)
    # Log targed kernel at new and old locations
    n.log.d<-sum(dexpgauss(y, mu = new.b0b1[1]+new.b0b1[2]*x, sigma=1, 
                           lambda=lam.cur, log = TRUE))-new.b0b1[1]^2/200-new.b0b1[2]^2/200
    o.log.d<-sum(dexpgauss(y, mu = b0.cur+b1.cur*x, sigma=1, 
                           lambda=lam.cur, log = TRUE))-b0.cur^2/200-b1.cur^2/200
    # Accept reject
    if(log(runif(1))<n.log.d-o.log.d){
      b0.cur<-new.b0b1[1]               # Store updates for first block (beta_0 and beta_1) 
      b1.cur<-new.b0b1[2]               # if proposal is accepted
      b1b2.acpt.cnt<-b1b2.acpt.cnt + 1  # Update acceptance count for block 1
      param.mat[i,1]<-b0.cur
      param.mat[i,2]<-b1.cur
    }
    else{
      param.mat[i,1]<-b0.cur        # Store updates for first block (beta_0 and beta_1)
      param.mat[i,2]<-b1.cur        # if proposal is rejected
    }     
    
    # Propose new lambda
    new.phi<-r.qphi(log(lam.cur),tau=tau.l$tphi)
    # Accept Rejct lambda
    n.log.d<-sum(dexpgauss(y, mu = b0.cur+b1.cur*x, sigma = 1, 
                           lambda=exp(new.phi), log = TRUE))+0.01*(new.phi-exp(new.phi))
    o.log.d<-sum(dexpgauss(y, mu = b0.cur+b1.cur*x, sigma = 1, 
                           lambda=lam.cur, log = TRUE))+0.01*(exp(lam.cur)-exp(exp(lam.cur)))
    
    if(log(runif(1))<n.log.d-o.log.d){
      lam.cur<-exp(new.phi)             
      lam.acpt.cnt<-lam.acpt.cnt + 1   # Update acceptance count for block 2
      param.mat[i,3]<-lam.cur          # Update second block (lambda) if accepted
      }
    else{
      param.mat[i,3]<-lam.cur          # Update second block if rejected
    }   
    
  }
  # Calculate acceptance rates
  acpt.rts<-list( b1b2.art=b1b2.acpt.cnt/N, lam.art = lam.acpt.cnt/N)
  # Return MC chain and acceptance rates
  return(list(param.mat= param.mat, acpt.rts=acpt.rts))
}




#######################################################################
### Function: denplot
### Usage: Wrapper for overlaying density plots for after N/2 and N 
#          samples from MCMC output
#
### Arguments:
# smpls: vector of samples for which to plot density estimates
########################################################################
denplot<-function(smpls,...){
  ylim<-range(density(smpls)$y,density(smpls[1:(length(smpls)/2)])$y)
  plot(density(smpls), main="Density Estimates", ylim=ylim,...) 
  lines(density(smpls[1:(length(smpls)/2)]), lty=2)
#   legend("topleft", # plot legend
#          legend=c("From N Samples","From N/2 Samples"),
#          bty="n", 
#          lty=c(1,2))
}

#######################################################################
### Function: over.est.plt
### Usage: Plot sequential estimates from 5 MCMC realizations on the 
#           same graph. Used for diagnosing an MCMC algorithm to 
#           ensure convergence from various starting locations
### Arguments:
# seq.ests: vector of sequential estimates calculated in increments of 
#           100
# ...:      legend graphing parameters
########################################################################
over.est.plt<-function(seq.ests,...){
  ns<-seq(100,100*dim(seq.ests)[1],by=100)  # Plot sample size on x-axis
  ylims<-range(seq.ests)            # Store range of samples for plotting window
  plot(ns,seq.ests[,1], ylim=ylims,
       main=paste("M.C. Estimates vs. Sample Size\n"),
       type="l",xlab="N",ylab="MC estimate",
       lty=1,col=1)
  lines(ns,seq.ests[,2],lty=2,col=2)
  lines(ns,seq.ests[,3],lty=3,col=4)
  lines(ns,seq.ests[,4],lty=4, col=10)
  lines(ns,seq.ests[,5],lty=5, col=9)
  legend(x="top", 
         bty="n", 
         lty=c(1,2,3,4,5),
         col=c(1,2,4,10,9),...)
}


#######################################################################
### Function: plot.mh
### Usage: MCMC diagnostic function for questions 2 and 3 which 
#          generates graphs of estimates against increasing sample size,
#          MCMCse by sample size, and ACF plots for beta_0, beta_1, and
#          lambda on a 3x3 grid
# 
#
### Arguments:
# param.mat:  Nx3 matrix where first column contains beta_0 samples,
#             the second column contains beta_1 samples, and 
#             the third column contains lambda samples
########################################################################

plot.mh<-function(param.mat){
  par(mfrow=c(3,3))  
  N<-dim(param.mat)[1]

  # Plot estimates against sample size
  btch.ests<-apply(param.mat, FUN = seq.est, g=mean,MARGIN = 2)    
  ns<-seq(100,N,by=100)  # Plot sample size on x-axis
  
  plot(ns,btch.ests[,1],  
       main=paste("B0 M.C. Estimates by N \n"),
       type="l",xlab=" Sample Size",ylab="MC estimate")
  plot(ns,btch.ests[,2],  
       main=paste("B1 M.C. Estimates by N \n"),
       type="l",xlab=" Sample Size",ylab="MC estimate")
  
  plot(ns,btch.ests[,3],  
       main=paste("Lambda M.C. Estimates by N \n"),
       type="l",xlab=" Sample Size",ylab="MC estimate")
  
  # Plot MCMCse against  Sample Size
  btch.ests<-apply(param.mat, FUN = seq.est, g=bm, MARGIN = 2, select.args="se")    
  plot(ns,btch.ests[,1],  
       main=paste("B0 MCMCse vs.  Sample Size\n"),
       type="l",xlab=" Sample Size",ylab="MCMCse")
  plot(ns,btch.ests[,2],  
       main=paste("B1 MCMCse vs.  Sample Size\n"),
       type="l",xlab=" Sample Size",ylab="MCMCse")
  
  plot(ns,btch.ests[,3],  
       main=paste("Lambda MCMCse vs.  Sample Size\n"),
       type="l",xlab=" Sample Size",ylab="MCMCse")
  
  # ACF Plots of samples
  acf(param.mat[,1], main="B0 Samples ACF")
  acf(param.mat[,2], main="B1 Samples ACF")
  acf(param.mat[,3],  main="Lambda Samples ACF")
  
}

###
### Begin Main for Questions 1,2,3
###

###
### Question 1 (a)
###
set.seed(19736482)
# Use EMG1.dat 
x<-dat1[,1]
y<-dat1[,2]

### Plot target kernel to inform proposal for MH algorithm
b1.grd<-seq(4.325738,10.32574,length.out=1000)
plot(b1.grd,exp(sapply(b1.grd,logd.b1)),type="l", 
          xlab="beta 1", ylab= "scaled target posterior")

# Run Metropolis Hastings Algorithm with 200,000 samples
fin.smps<-mhsampler1(7.338068, N=200000, tau=0.75)

### 
### Question 1 (b)  
### 
# Posterior Expectation and SE
bm(fin.smps) 

###
### Question 1 (c) 
###
# 95% Credible Intervals for B_1
quantile(fin.smps,c(0.025,0.975))    

###
### Question 1 (d)
###
# Overlay estimated density at N and N/2
denplot(fin.smps, "Smoothed Density Estimate", xlab = "B1")

###
### Question 1 (e)
###
par(mfrow=c(1,2))

# Trace plot of samples: check mixing
plot(fin.smps,type="l", xlab="N", ylab="Sample Beta 1", main="Trace Plot")

# Check autocorrelation of samples
acf(fin.smps, main="Sample Autocorrelation")


par(mfrow=c(1,3))
### Calculate Convergence of Mean Estimate
seq.ests<-seq.est(fin.smps, g=mean)    
plot(seq(100,length(fin.smps),by=100),seq.ests,  
     main=paste("B1 M.C. Estimates vs. Sample Size\n"),
     type="l",xlab=" Sample Size",ylab="MC estimate")


### Plot MCMCse by N
seq.ests<-seq.est(fin.smps, g=bm, select.args = "se")     
plot(seq(100,length(fin.smps),by=100),seq.ests,  
     main=paste("B1 MCMCse vs.  Sample Size\n"),
     type="l",xlab="N",ylab="MCMCse")

# Calculate Effective Sample Size 
ess(fin.smps)

### Run M-H for several different starting values of Beta_1
smps1<-mhsampler1(-2.6)
smps2<-mhsampler1(3)
smps3<-mhsampler1(7.3)
smps4<-mhsampler1(12)
smps5<-mhsampler1(17.3)

smp.mat<-cbind(smps1,smps2,smps3,smps4,smps5)   # Store samples in a matrix

# Calculate Sequential estimates as n grows for both chains
seq.ests<-apply(smp.mat, FUN = seq.est, MARGIN = 2, g=mean)    

# Plot Estimates as a function of sample size
over.est.plt(seq.ests,
legend=c("Initial Value: -2.6","Initial Value: 3","Initial Value: 7.3",
         "Initial Value: 12","Initial Value: 17.3"),cex=1.5)



### 
### Question 2
### 
set.seed(19736482)
# Use EMG2.dat 
x<-dat2[,1]
y<-dat2[,2]

### 
### Question 2 (a) 
### 
# Run Metropolis Hastings Algorithm with 200,000 samples
mh2<-mhsampler2(N=200000, init.param=c(2.346,3.46,0.803), tau.l=list(tb0=0.2, tb1=0.3, tphi=0.1))

### 
### Question 2 (b) 
### 

# Estimates and MCMCse 
apply(mh2$param.mat, FUN =bm,MARGIN = 2) 

# 95% Credible Intervals
quantile(mh2$param.mat[,1],c(0.025,0.975))    
quantile(mh2$param.mat[,2],c(0.025,0.975))    
quantile(mh2$param.mat[,3],c(0.025,0.975))    


### 
### Question 2 (c)
### 
# Estimate Correlation between Beta 0 and Beta 1
cor(mh2$param.mat)

### 
### Question 2 (d)
### 

# Density Plots
# Overlay density at N and N/2
par(mfrow=c(1,3))
denplot(mh2$param.mat[,1], xlab="B0")
denplot(mh2$param.mat[,2], xlab="B1")
denplot(mh2$param.mat[,3], xlab="Lambda")


### 
### Question 2 (e)
### 
## Calculate Effective Sample Size
ess(mh2$param.mat[,1])
ess(mh2$param.mat[,2])
ess(mh2$param.mat[,3])

# Plot Samples against N (Trace Plot)
plot(mh2$param.mat[,1],type="l", ylab="B0 Sample", xlab="N")
plot(mh2$param.mat[,2],type="l",ylab="B1 Sample", xlab="N")
plot(mh2$param.mat[,3],type="l",ylab="Lambda Sample", xlab="N")

# Diagnostic Plots
plot.mh(mh2$param.mat)

# Calculate acceptance rates
mh2$acpt.rts

# Consider Different Starting Values
ms1<-mhsampler2(N=10000, init.param=c(-3,1.5,5), tau.l=list(tb0=0.2, tb1=0.3, tphi=0.1))
ms2<-mhsampler2(N=10000, init.param=c(0.35,5,0.5), tau.l=list(tb0=0.2, tb1=0.3, tphi=0.1))
ms3<-mhsampler2(N=10000, init.param=c(2,-3, 1.5), tau.l=list(tb0=0.2, tb1=0.3, tphi=0.1))
ms4<-mhsampler2(N=10000, init.param=c(4,3.5,0.8), tau.l=list(tb0=0.2, tb1=0.3, tphi=0.1))
ms5<-mhsampler2(N=10000, init.param=c(12,7,0.3), tau.l=list(tb0=0.2, tb1=0.3, tphi=0.1))


# Generate Plots to check convergence from different starting locations
par(mfrow=c(1,3))
smp.mat<-cbind(ms1$param.mat[,1],ms2$param.mat[,1],ms3$param.mat[,1],ms4$param.mat[,1],ms5$param.mat[,1])   # Store samples in a matrix
seq.ests<-apply(smp.mat, FUN = seq.est, MARGIN = 2, g=mean)    
over.est.plt(seq.ests, legend=c("Initial Value: -3","Initial Value: 0.35","Initial Value: 2",
                                "Initial Value: 4","Initial Value: 12"), cex=1.8)

smp.mat<-cbind(ms1$param.mat[,2],ms2$param.mat[,2],ms3$param.mat[,2],ms4$param.mat[,2],ms5$param.mat[,2])   # Store samples in a matrix
seq.ests<-apply(smp.mat, FUN = seq.est, MARGIN = 2, g=mean)    
over.est.plt(seq.ests,legend=c("Initial Value: 1.5","Initial Value: 5","Initial Value: -3",
                               "Initial Value: 3.5","Initial Value: 7"), cex=1.8)

smp.mat<-cbind(ms1$param.mat[,3],ms2$param.mat[,3],ms3$param.mat[,3],ms4$param.mat[,3],ms5$param.mat[,3])   # Store samples in a matrix
seq.ests<-apply(smp.mat, FUN = seq.est, MARGIN = 2, g=mean)    
over.est.plt(seq.ests,legend=c("Initial Value: 5","Initial Value: 0.5","Initial Value: 1.5",
                               "Initial Value: 0.8","Initial Value: 0.3"), cex=1.8)
###
### Question 3
###
set.seed(19736482)
# Use EMG3.dat 
x<-dat3[,1]
y<-dat3[,2]

# Run Metropolis Hastings Algorithm with 200,000 samples
mh3<-mhsampler3(N=200000, init.param=c(0.148,2.474,0.161), tau.l=list(tb0=0.325, tb1=0.569, tphi=0.143))

# Estimates and MCMCse 
apply(mh3$param.mat, FUN =bm,MARGIN = 2) 

# 95% Credible Intervals
quantile(mh3$param.mat[,1],c(0.025,0.975))    
quantile(mh3$param.mat[,2],c(0.025,0.975))    
quantile(mh3$param.mat[,3],c(0.025,0.975))  
### 
### Question 3 (b) 
### 

# Density Plots
# Overlay density at N and N/2
par(mfrow=c(1,3))
denplot(mh3$param.mat[,1], xlab="B0")
denplot(mh3$param.mat[,2], xlab="B1")
denplot(mh3$param.mat[,3], xlab="Lambda")


##
## Check other Criteria
##
# Correlation of samples
cor(mh3$param.mat)

## Calculate Effective Sample Size
ess(mh3$param.mat[,1])
ess(mh3$param.mat[,2])
ess(mh3$param.mat[,3])

# Plot Samples against N (Trace Plot)
plot(mh3$param.mat[,1],type="l", ylab="B0 Sample", xlab="N")
plot(mh3$param.mat[,2],type="l",ylab="B1 Sample", xlab="N")
plot(mh3$param.mat[,3],type="l",ylab="Lambda Sample", xlab="N")

# Diagnostic Plots
plot.mh(mh3$param.mat)

# Calculate acceptance rates
mh3$acpt.rts

# Consider Different Starting Values
ms1<-mhsampler3(N=10000, init.param=c(-3,1.5,5), tau.l=list(tb0=0.325, tb1=0.569, tphi=0.143))
ms2<-mhsampler3(N=10000, init.param=c(0.35,5,0.5), tau.l=list(tb0=0.325, tb1=0.569, tphi=0.143))
ms3<-mhsampler3(N=10000, init.param=c(2,-3, 1.5), tau.l=list(tb0=0.325, tb1=0.569, tphi=0.143))
ms4<-mhsampler3(N=10000, init.param=c(4,3.5,0.8), tau.l=list(tb0=0.325, tb1=0.569, tphi=0.143))
ms5<-mhsampler3(N=10000, init.param=c(12,7,0.3), tau.l=list(tb0=0.325, tb1=0.569, tphi=0.143))

par(mfrow=c(1,3))
smp.mat<-cbind(ms1$param.mat[,1],ms2$param.mat[,1],ms3$param.mat[,1],ms4$param.mat[,1],ms5$param.mat[,1])   # Store samples in a matrix
seq.ests<-apply(smp.mat, FUN = seq.est, MARGIN = 2, g=mean)    
over.est.plt(seq.ests, legend=c("Initial Value: -8","Initial Value: 0.35","Initial Value: 2",
                                "Initial Value: 4","Initial Value: 12"))

smp.mat<-cbind(ms1$param.mat[,2],ms2$param.mat[,2],ms3$param.mat[,2],ms4$param.mat[,2],ms5$param.mat[,2])   # Store samples in a matrix
seq.ests<-apply(smp.mat, FUN = seq.est, MARGIN = 2, g=mean)    
over.est.plt(seq.ests,legend=c("Initial Value: -8","Initial Value: 0.35","Initial Value: 2",
                               "Initial Value: 4","Initial Value: 12"))

smp.mat<-cbind(ms1$param.mat[,3],ms2$param.mat[,3],ms3$param.mat[,3],ms4$param.mat[,3],ms5$param.mat[,3])   # Store samples in a matrix
seq.ests<-apply(smp.mat, FUN = seq.est, MARGIN = 2, g=mean)    
over.est.plt(seq.ests,legend=c("Initial Value: -8","Initial Value: 0.35","Initial Value: 2",
                               "Initial Value: 4","Initial Value: 12"))


