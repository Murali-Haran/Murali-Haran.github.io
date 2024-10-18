###################################################################################
#######                           PROBLEM 1
###################################################################################
##Read in the data and label columns "X" and "Y"
regression1=read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
colnames(regression1)=c("X","Y")
X=regression1$X
Y=regression1$Y

## Sources
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")
source("http://www.stat.psu.edu/~mharan/batchmeans.R")

## Posterior is proportional to the likelihood of Beta_1 and X times the prior of Beta_1
## The log of the posterior is used for stability of computations
log.posterior=function(X,Y,b1)
{
  b0=5
  lambda=0.4
  sigma=1
  sum(dexpgauss(Y,mu = (b0+b1*X),sigma = sigma,lambda = lambda,log = TRUE))
  -(1/2)*log(200*pi)-b1^2/200
}

#MCMC algorithm using Metropolis update for Beta_1
MCMC=function(n.sims,tau.2,b1.init) 
{
  ## Create an empty vector in which to store the chain of Beta_1 values
  ## Also create an empty vector for the accepted values, which will be useful for acceptance rate
  #Choose an initial value for Beta_1, say Beta_1=0 since E(Beta_1)=0 based on its prior distr.
  mchain=rep(NA,n.sims)
  accepted=rep(0,n.sims)
  mchain[1]=b1.init
  
  for(i in 1:(n.sims-1))
  {
    ## Using a Random Walk M-H algorithm, the chosen proposal distribution is q~N(Beta_1.current,tau^2)
    ## So Beta_1.proposal~N(Beta_1.current,tau^2), where tau^2 is the tuning parameter
    b1.curr=mchain[i]
    b1.prop=rnorm(1,mean = b1.curr,sd = tau.2)
    
    ## Acceptance probability is alpha=min(1, h(b1.prop)/h(b1.curr))
    ## So log(alpha)=min(0,log(h(b1.prop))-log(h(b1.curr)))
    log.prob=min(0,(log.posterior(X,Y,b1.prop)-log.posterior(X,Y,b1.curr)))
    
    ## Draw a U~Uniform(0,1) r.v. and accept proposal Beta_1 if U<alpha, or log(U)<log(alpha)
    ## Otherwise, set new Beta_1 value as the current Beta_1 value
    if(log(runif(n = 1,min = 0,max = 1))<log.prob)
    {
      mchain[i+1]=b1.prop
      accepted[i+1]=1
    }
    else
    {
      mchain[i+1]=b1.curr
    }
  }
  accept.rate=sum(accepted)/n.sims
  return(list(mchain=mchain,accept.rate=accept.rate))
}

set.seed(1)

## Final Chain used for Problem 1
chain.ex5=MCMC(1000000,25,0)
mchain5=chain.ex5$mchain
accept5=chain.ex5$accept.rate
par(mfrow=c(1,1))
acf(mchain5) # Check the autocorrelation of the chain's values over different lags
estvssamp(samp = mchain5) # reasonable convergence of MC estimate over increasing n
bm(mchain5) # MCMC se for the estimate is fairly small
ess(mchain5) # Out of sample size of 1,000,000, effective sample size is 230,006.4
accept5 # Acceptance rate of the chain is 43%

#################################
###### Diagnostics ##############
#################################

### ESS of 224,912.7 out of 1,000,000 sample size

### Start chain at different initial values and evaluate the estimate
plot(estplot1,type='l',ylim=c(-0.7,0.9),lwd=2)
lines(estplot2,col="red",lwd=3)
lines(estplot3,col="green",lwd=2)
lines(estplot4,col="blue",lwd=2)
abline(h=0,lty=2)

### MCMCse over different n
n=seq(from = 1000,to = 10000,by=500)
mcmc.mat=matrix(NA,nrow = length(n),ncol = 2)
for(i in 1:length(n))
{
  sample=MCMC(n[i],25,0)
  estimate=bm(sample$mchain)$est
  se=bm(sample$mchain)$se
  mcmc.mat[i,1]=estimate
  mcmc.mat[i,2]=se
}
plot(x=n,y=mcmc.mat[,2],ylab="MCMC s.e.",xlab="MCMC sample size")

### acf plot for autocorrelation
acf(mchain5)

## Estimate of posterior E(Beta_1)
b1.estimate=mean(mchain5)
b1.estimate

## 95% Credible interval for Beta_1
quantile(mchain5,c(.025,.975))

## Plot of the estimated posterior pdf of Beta_1
plot(density(mchain5))
abline(v=print(quantile(mchain5, c(.025, .975))), lty=2, lwd=3,col="red")

###################################################################################
#######                           PROBLEM 2
###################################################################################
##Read in the data and label columns "X" and "Y"
regression2=read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
colnames(regression2)=c("X","Y")
X=regression2$X
Y=regression2$Y
sigma=1

## Set seed for reproducibility
set.seed(2)

## Joint density of (Beta_0,Beta_1,Lambda,X,Y), given below, is proportional to any of the full conditional distributions
log.joint=function(b0,b1,lambda,X,Y) 
{
  sigma=1
  sum(dexpgauss(Y,mu = (b0+b1*X),sigma = sigma,lambda = lambda,log = TRUE))+dnorm(b0,0,10,log = TRUE)+dnorm(b1,0,10,log = TRUE)+dgamma(lambda,shape = 0.01,scale = 100,log = TRUE)
}

##### Create the Metropolis-Hastings Sampler
mhsampler = function(NUMIT,dat=Y,tau2,inits)
{
  n = length(dat)
  cat("n=",n,"\n")
  ## set up
  ## 3 x NUMIT matrix to store Markov chain values
  ## each row corresponds to one of 3 parameters in order: Beta_0,Beta_1,Lambda
  ## each column corresponds to a single state of the Markov chain
  mchain = matrix(NA, 3, NUMIT)
  acc.b0 = 0 # count number of accepted proposals for Beta_0
  acc.b1 = 0 # count number of accepted proposals for Beta_1
  acc.lam = 0 # count number of accepted proposals for Lambda
  
  ## starting values for Markov chain
  ## This is somewhat arbitrary but any method that produces reasonable values for each parameter is usually adequate.
  ## For instance, we can use approximate prior means or approximate MLEs.
  
  ## The function argument inits is a vector of inital values for Beta_0,Beta_1,and Lambda respectively
  b0.init = inits[1]
  b1.init = inits[2] 
  lam.init = inits[3]
  mchain[,1] = c(b0.init,b1.init,lam.init)
  
  ## The function argument tau2 is a vector of tuning parameters for Beta_0,Beta_1,and Lambda, respectively
  tau2.a=tau2[1]
  tau2.b=tau2[2]
  tau2.c=tau2[3]
  
  for (i in 2:NUMIT)
  {
    ## most upto date state for each parameter
    curr.b0 = mchain[1,i-1]
    curr.b1 = mchain[2,i-1]
    curr.lam = mchain[3,i-1]    
    
    ## sample from full conditional distribution of Beta_0 (Metropolis-Hastings update)
    prop.b0 = rnorm(n = 1,mean = curr.b0,sd = tau2.a)
    
    ## Metropolis accept-reject step (in log scale)
    logMHratio = log.joint(prop.b0,curr.b1,curr.lam,X,Y)-log.joint(curr.b0,curr.b1,curr.lam,X,Y)
    
    logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
    if (log(runif(1))<logalpha) # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    {
      acc.b0 = acc.b0 + 1 # increment count of accepted proposals
      curr.b0 = prop.b0
    }
    
    ## sample from full conditional distribution of Beta_1 (Metropolis-Hastings update)
    prop.b1 = rnorm(n = 1,mean = curr.b1,sd = tau2.b)
    
    ## Metropolis accept-reject step (in log scale)
    logMHratio = log.joint(curr.b0,prop.b1,curr.lam,X,Y)-log.joint(curr.b0,curr.b1,curr.lam,X,Y)
    
    logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
    if (log(runif(1))<logalpha) # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    {
      acc.b1 = acc.b1 + 1 # increment count of accepted proposals
      curr.b1 = prop.b1
    }
    
    ## sample from full conditional distribution of Lambda (Metropolis-Hastings update)
    prop.lam = exp(rnorm(n = 1,mean = log(curr.lam),sd = tau2.c))
    
    ## Metropolis accept-reject step (in log scale)
    logMHratio = 
    {
      log.joint(curr.b0,curr.b1,prop.lam,X,Y)-log.joint(curr.b0,curr.b1,curr.lam,X,Y)
    }
    
    logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
    if (log(runif(1))<logalpha) # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
    {
      acc.lam = acc.lam + 1 # increment count of accepted proposals
      curr.lam = prop.lam
    }
    
    ## update chain with new values
    mchain[,i] = c(curr.b0,curr.b1,curr.lam)
    
      }
    
    acc.rate=c(acc.b0/(NUMIT-1),acc.b1/(NUMIT-1),acc.lam/(NUMIT-1))
    cat("Markov chain algorithm ran for",NUMIT,"iterations\n")
    cat("Acceptance rates for Beta_0,Beta_1, and Lambda, respectively =",acc.rate,"\n")
    cat("Parameters are in order: Beta_0, Beta_1, Lambda\n")
    return(mchain)
}


##### Final chain used for Problem 2
chain.2=mhsampler(1000000,tau2=c(.40,.45,.13),inits=c(.5,.5,1.5))
chain2.est=bmmat(chain.2)# MCMC se looks good for each estimate and ESS is well over 5000 for each parameter
acf(chain.2[1,])
acf(chain.2[2,])
acf(chain.2[3,])

## Estimates of the posterior means and MCMC se for Beta_0,Beta_1,and Lambda
chain2.est

## 95% credible intervals for each parameter
quantile(chain.2[1,],c(0.025,0.975)) # Interval for Beta_0
quantile(chain.2[2,],c(0.025,0.975)) # Interval for Beta_1
quantile(chain.2[3,],c(0.025,0.975)) # Interval for Lambda

## Estimated correlation between Beta_0 and Beta_1
cor(chain.2[1,],chain.2[2,])

## Density plots of respective marginal distributions of Beta_0,Beta_1,and Lambda
plot(density(chain.2[1,])) # Smooth density plot for Beta_0
plot(density(chain.2[2,])) # Smooth density plot for Beta_1
plot(density(chain.2[3,])) # Smooth density plot for Lambda

###################################################################################
#######                           PROBLEM 3
###################################################################################
##Read in the data and label columns "X" and "Y"
regression3=read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")
colnames(regression3)=c("X","Y")
X=regression3$X
Y=regression3$Y
sigma=1

## Set seed for reproducibility
set.seed(3)

####################################
### Adjusting the bmmat function ###
####################################
bmmat=function(mcmat,skip=NA)
{
  if (!any(is.na(skip)))
  {
    num=ncol(mcmat)-length(skip)
    mcmat=mcmat[-skip] # remove columns to be skipped
  }
  else # assume it is NA
    num=nrow(mcmat)
  
  bmvals=matrix(NA,num,3,dimnames=list(c("Beta_0","Beta_1","Lambda"),c("est","se","ess"))) # first col=est, second col=MS s.error
  
  bmres=apply(mcmat,1,bm)
  for (i in 1:num)
  {
    bmvals[i,]=c(bmres[[i]]$est,bmres[[i]]$se,ess(mcmat[i,]))
  }
  return(bmvals)
}

## Final Chain for Problem 3
chain.3=mhsampler(200000,Y,tau2=c(0.4,0.4,0.1),inits=c(0.5,0.5,1.5))
chain3.est=bmmat(chain.3)

## Estimates of the posterior means and MCMC se for Beta_0,Beta_1,and Lambda
chain3.est

## 95% credible intervals for each parameter
quantile(chain.3[1,],c(0.025,0.975)) # Interval for Beta_0
quantile(chain.3[2,],c(0.025,0.975)) # Interval for Beta_1
quantile(chain.3[3,],c(0.025,0.975)) # Interval for Lambda

## Estimated correlation between Beta_0 and Beta_1
cor(chain.3[1,],chain.2[2,])

## Density plots of respective marginal distributions of Beta_0,Beta_1,and Lambda
plot(density(chain.3[1,])) # Smooth density plot for Beta_0
plot(density(chain.3[2,])) # Smooth density plot for Beta_1
plot(density(chain.3[3,])) # Smooth density plot for Lambda