## ----P1_functions,echo=FALSE---------------------------------------------
# Title: Take Home Final exam Stat 515 sp 2015
# Author: Abhishek Rao

# Format: The code is structured as divisions of #
# 1) constants and data
# 2) Functions
# 3) Main


############# Constants and data ##################################
options(digits=4)
sample_size = 33123
start_value_range = c(-10,0,10) # try for different values of starting value
tau_range = c(1,2)  # Try for different variance of proposal function
discard_length = sample_size/10 # while checking the acceptance rate, discard
# the first this much samples.
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")
source("http://www.stat.psu.edu/~mharan/batchmeans.R")
X_and_Y = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
X_i = X_and_Y$V1
Y_i = X_and_Y$V2
n_data = length(Y_i)
par(mar=rep(2,4))

########### Functions ###################################
## plot how Monte Carlo estimates change with increase in sample size (Modified by Abhishek
# added ylimit)
## input: samp (sample vector) and g (where E(g(x)) is quantity of interest)
## output: plot of estimate over time (increasing sample size)
## e.g.: estvssamp(outp,your_plot=plot))
estvssamp2 = function(samp, plotname="mean estimates",ylim)
{
  if (length(samp)<100)
    batchsize = 1
  else
    batchsize = length(samp)%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp),by=batchsize))
  {
    est = c(est, mean(samp[1:i]))
  }
  
  #    plot(seq(batchsize,length(samp),by=batchsize),est,main=paste("M.C. estimates vs. sample size\n"),type="l",xlab="sample size",ylab="MC estimate")
  plot(seq(batchsize,length(samp),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MC estimate",ylim=ylim)
}

# Slightly modified version of the orginal, now it's lines instead of plot
estvssamplines = function(samp, plotname="mean estimates",ylim)
{
  if (length(samp)<100)
    batchsize = 1
  else
    batchsize = length(samp)%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp),by=batchsize))
  {
    est = c(est, mean(samp[1:i]))
  }
  
  #    plot(seq(batchsize,length(samp),by=batchsize),est,main=paste("M.C. estimates vs. sample size\n"),type="l",xlab="sample size",ylab="MC estimate")
  lines(seq(batchsize,length(samp),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MC estimate",ylim=ylim)
}

# The MCMC function
# Inputs: sample_size is the chain length
#         distribution_h_l is the h(x) distribution to take samples from
run_MCMC <- function(samples_size, distribution_h_l, start_value=1,  tau=1){
  chain = rep(NA,samples_size)
  chain[1] = start_value
  # start the MCMC chain
  for (i in 2:samples_size){
    y = rnorm(1,mean=chain[i-1],sd=tau)
    acceptance_probability = exp(distribution_h_l(y) - distribution_h_l(chain[i-1]))
    if (runif(1) < acceptance_probability){
      # accept case
      chain[i] = y
    } else {
      # reject case
      chain[i] = chain[i-1]
    }
  }
  return(chain)
}

# uses the run_MCMC function 3 times for different start values and creates plots.
experiment_MCMC <- function(tau=1){
  chain_1 =run_MCMC(sample_size,posterior,start_value=start_value_range[1], tau=tau)
  chain_2 =run_MCMC(sample_size,posterior,start_value=start_value_range[2], tau=tau)
  chain_3 =run_MCMC(sample_size,posterior,start_value=start_value_range[3], tau=tau)
  # Plot 3 estimates that start with different values
  yrange <- c(6,8)
  par(col="red")
  estvssamp2(chain_1,plotname=paste("E(beta_1) for tau=",tau),ylim=yrange)
  par(col=33)
  estvssamplines(chain_2,ylim=yrange)
  par(col=29)
  estvssamplines(chain_3,ylim=yrange)
  par(new=FALSE,col="red")
  return(chain_2)
}


############# Distributions ##########################
# The loglikelihood for the given problem
single_likelihood = function(Yi,Xi,b)
  dexpgauss(Yi, mu=5+b*Xi, sigma=1, lambda=0.4, log=TRUE)

# The posterior function in log scale
posterior <- function(b){
  sum(single_likelihood(Y_i,X_i,rep(b,n_data))) + dnorm(b,mean=0,sd=10,log=TRUE)
}

## ----Main1,echo=FALSE,fig.height=4,fig.cap="Plot of Estimate vs iteration number",cache=TRUE----
# Problem 1 Part 1
par(mfrow=c(1,2))
chain_tau1 = experiment_MCMC(tau=tau_range[1])
bm1 = bm(chain_tau1)
est1 = bm1$est  # Get the estimate from batchmeans code.
se1 = bm1$se  # Get the MCSE from batchmeans code

chain_tau2 = experiment_MCMC(tau=tau_range[2])
bm2 = bm(chain_tau2)
est2 = bm2$est
se2 = bm2$se

## ----Density,fig.height=5,echo=FALSE,fig.cap="Density and ACF plots for Problem1",cache=TRUE----
par(mfrow = c(2,2))
plot(density(chain_tau1),main=paste("Density for tau = ",tau_range[1]))
plot(density(chain_tau2),main=paste("Density for tau = ",tau_range[2]))
acf(chain_tau1)
acf(chain_tau2)

## ----Prob2_init,echo=FALSE,cache=TRUE------------------------------------
############# Constants and data ##################################
# Variance multiplier for proposal function for each parameter
#set.seed(235)
sample_size2 = 1e5
beta_0_f = 1
beta_1_f = 2
lambda_f = 1/3
# Set of starting values, each column is a different draw, rows correspond to beta0,beta1, lambda
p2_start_value_range = matrix(c(rnorm(3,mean=0,sd=10),rnorm(3,mean=0,sd=10),rgamma(3,shape=0.01,scale=100)),ncol=3,byrow=TRUE)
yrangeb <- c(-3,5)  # range for beta_0, beta_1 plot
yrangel <- c(0,2)  # range for lambda plot
tau_range = c(0.3)

data_2 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
X_i = data_2$V1
Y_i = data_2$V2
n_data = length(X_i)
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")
source("http://www.stat.psu.edu/~mharan/batchmeans.R")


############# Distributions ##########################
# The loglikelihood for the given problem
single_likelihood2 = function(Yi,Xi,params){
  beta0 = params[1]
  beta1 = params[2]
  lambda = params[3]
  dexpgauss(Yi, mu=beta0+beta1*Xi, sigma=1, lambda=lambda, log=TRUE)
}

# prior function
log_prior <-function(params){
  beta0 = params[1]
  beta1 = params[2]
  lambda = params[3]
  return(dnorm(beta0,mean=0,sd=10,log=TRUE)+dnorm(beta1,mean=0,sd=10,log=TRUE)+dgamma(lambda,shape=0.001,scale=100,log=TRUE))
}

# The posterior function in log scale
posterior2 <- function(params){
  if(params[3]<0)
    return(-1e99)  # Give 0 probability for lambda < 0
  sum(single_likelihood2(Y_i,X_i,rep(params,n_data))) + log_prior(params)
}

# Proposal function
proposal_fx <-function(current_value,tau){
  c( rnorm(1,mean=current_value[1],sd=tau*beta_0_f),
     rnorm(1,mean=current_value[2],sd=tau*beta_1_f),
     rnorm(1,mean=current_value[3],sd=tau*lambda_f))
}


############### Functions ###########################
run_MCMC2 <- function(samples_size, distribution_h_l, start_value=c(1,1,1),  tau=1){
  variance_matrix = diag(c(tau,tau,tau)) # for the proposal function
  chain = matrix(NA,3, samples_size)  # create empty matrix
  # the columns are the iteration number and row(1,2,3)=(beta0,beta1,lambda) 
  chain[,1] = start_value
  # start the MCMC chain
  for (i in 2:samples_size){
    # proposal function
    y =  proposal_fx(current_value=chain[,i-1],tau=tau)
    acceptance_probability = exp(distribution_h_l(y) - distribution_h_l(chain[,i-1]))
    if (runif(1) < acceptance_probability){
      # accept case
      chain[,i] = y
    } else {
      # reject case
      chain[,i] = chain[,i-1]
    }
  }
  return(chain)
}

plot_3chains <-function(pchain_1,pchain_2,pchain_3,plotname,tau,yrange){  
  estvssamp2(pchain_1,plotname=plotname,ylim=yrange)
  par(col=33)
  estvssamplines(pchain_2,ylim=yrange)
  par(col=29)
  estvssamplines(pchain_3,ylim=yrange)
  par(new=FALSE,col="red")
}

experiment_MCMC2 <- function(tau=1){
  chain_1 =run_MCMC2(sample_size2, distribution_h_l=posterior2, start_value=p2_start_value_range[,1], tau=tau)
  chain_2 =run_MCMC2(sample_size2, distribution_h_l=posterior2, start_value=p2_start_value_range[,2], tau=tau)
  chain_3 =run_MCMC2(sample_size2, distribution_h_l=posterior2, start_value=p2_start_value_range[,3], tau=tau)
  # Plot 3 estimates that start with different values
  par(col="red",mfrow=c(3,2))
  plot_3chains(chain_1[1,],chain_2[1,],chain_3[1,],plotname=paste("E(beta_0) for tau=",tau),tau=tau,yrange=yrangeb)
  acf(chain_2[1,])
  plot_3chains(chain_1[2,],chain_2[2,],chain_3[2,],plotname=paste("E(beta_1) for tau=",tau),tau=tau,yrange=yrangeb)
  acf(chain_2[2,])
  plot_3chains(chain_1[3,],chain_2[3,],chain_3[3,],plotname=paste("E(lambda) for tau=",tau),tau=tau,yrange=yrangel)
  acf(chain_2[3,])
  return(chain_2)
}

## ----Main2,echo=FALSE,fig.height=9,fig.cap="Estimates vs iterations and their corresponding autocorrelation for Problem 2",cache=TRUE----
chain_21 = experiment_MCMC2(tau=tau_range[1])

## ----Densities2,echo=FALSE,fig.height=5,fig.cap="Density plots for Problem 2",cache=TRUE----
options(digits=4)
par(mfrow=c(2,2))
plot(density(chain_21[1,]),main=expression(paste("Density of ",beta,"0")))
plot(density(chain_21[2,]),main=expression(paste("Density of ",beta,"1")))
plot(density(chain_21[3,]),main=expression(paste("Density of ",lambda)))

## ----Prob3_init,echo=FALSE,cache=TRUE------------------------------------
############# Constants and data ##################################
sample_size2 = 1e5
yrange <- c(-3,5)
tau_range = c(0.08)
# Set of starting values, each column is a different draw, rows correspond to beta0,beta1, lambda
#set.seed(222)
p2_start_value_range = matrix(c(rnorm(3,mean=0,sd=10),rnorm(3,mean=0,sd=10),rgamma(3,shape=0.01,scale=100)),ncol=3,byrow=TRUE)
# Variance multiplier for proposal function for each parameter
beta_0_f = 4
beta_1_f = 8
lambda_f = 1/8
data_3 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")
X_i = data_3$V1
Y_i = data_3$V2
n_data = length(X_i)


############# Distributions ##########################
# The loglikelihood for the given problem
single_likelihood2 = function(Yi,Xi,params){
  beta0 = params[1]
  beta1 = params[2]
  lambda = params[3]
  dexpgauss(Yi, mu=beta0+beta1*Xi, sigma=1, lambda=lambda, log=TRUE)
}

# prior function
log_prior <-function(params){
  beta0 = params[1]
  beta1 = params[2]
  lambda = params[3]
  return(dnorm(beta0,mean=0,sd=10,log=TRUE)+dnorm(beta1,mean=0,sd=10,log=TRUE)+dgamma(lambda,shape=0.001,scale=100,log=TRUE))
}

# The posterior function in log scale
posterior2 <- function(params){
  if(params[3]<0)
    return(-1e99)  # Give 0 probability for lambda < 0
  sum(single_likelihood2(Y_i,X_i,rep(params,n_data))) + log_prior(params)
}

# Proposal function
proposal_fx <-function(current_value,tau){
  c( rnorm(1,mean=current_value[1],sd=tau*beta_0_f),
     rnorm(1,mean=current_value[2],sd=tau*beta_1_f),
     rnorm(1,mean=current_value[3],sd=tau*lambda_f))
}



## ----Main_3,fig.cap="Plot of Estimate vs iteration number and the corresponding autocorrelation for Problem 3",echo=FALSE,cache=TRUE----

chain_21 = experiment_MCMC2(tau=tau_range[1])

## ----Densities3,echo=FALSE,fig.height=5,fig.cap="Density plots for Problem 3",cache=TRUE----
options(digits=4)
par(mfrow=c(2,2))
plot(density(chain_21[1,]),main=expression(paste("Density of ",beta,"0")))
plot(density(chain_21[2,]),main=expression(paste("Density of ",beta,"1")))
plot(density(chain_21[3,]),main=expression(paste("Density of ",lambda)))

