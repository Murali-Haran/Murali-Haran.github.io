########################
#
# STAT 515 - Final Take home exam 
# 
#     Ardalan Mirshani
#
########################



# getting data from website

data1= read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")

source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")
source("http://sites.stat.psu.edu/~mharan/batchmeans.R")


# mhsampler is a Metropolis All-at-once Hastings Algorithm for beta1 in #run=n and from data=dat

mhsamplera<-function(n,dat,tau, beta1init) { # tau is sd of proposal dist in random walk metropolis updates
   
  Y=dat[,2]   # predictors for Data 1
  X=dat[,1]   # Responses  for Data 2
  
  
  beta0=5
  sigma=1
  lambda=0.4
  
  
  mchain=rep(NA,n)   # markov chain for beta1
  acc=0              # acceptance rate
  

  # setting initial value
  
  mchain[1]=beta1init
  
  for(i in 2:n){
    currbeta1=mchain[i-1]
    
    ### sample from full canditional distribution of beta1
    propbeta1=rnorm(n = 1,mean = currbeta1,sd = tau)
    
    # producing acceptance ratio in log-scale
    logMHratio=dnorm(x = propbeta1,mean = 0,sd = 10,log = TRUE)+
      sum(dexpgauss(x = Y,mu = beta0+propbeta1*X,sigma =  sigma, lambda =  lambda,log = TRUE))-
      dnorm(x = currbeta1,mean = 0,sd = 10,log = TRUE)-
      sum(dexpgauss(x = Y,mu = beta0+currbeta1*X,sigma =  sigma, lambda =  lambda,log = TRUE))
    
    
    logalpha=min(log(1),logMHratio)
    
    
    # accept or reject part
    
    if(log(runif(1))<=logalpha) { # accept with prob. alpha
     acc=acc+1 
     mchain[i]=propbeta1
    }
    else{   # when  we reject the sample
      mchain[i]=currbeta1
    }
  }
  
  
  return(list(acc.r=acc/n,mchain=mchain))

  
}


####### part e

set.seed(18)
# we check different initial values and tau till reach this optimol values for acf and MCMC s.error
t=mhsamplera(n = 30000,dat = data1,tau = 0.67,beta1init = 7.2)
#t$mchain


# acceptance rate

t$acc.r  # we see this acceptance rate is between 25% and 50%
acf(t$mchain,main="Autocorrelation Function for Samples of beta1") 
# there is no heavily autocorellation function


ess(t$mchain) # the estimate effective sample is greater than 5000

####### part b

# produce point mean estimate and MCMC s.error
bm(t$mchain) 


####### part c

# finding 95% credible interval
mySamples=t$mchain
quantile(mySamples,c(0.025,0.975))   # 95% credible interval based on samples

####### part d

# density plots for samples

par(mfrow=c(1,2))
plot(density(mySamples),main=" density of the posterior function of beta1", ylab="beta1")
plot(density(mySamples[((length(mySamples)/(4/3)):length(mySamples))]),
     main=" density of posterior function of beta1 after Burn-in Process \n keep last 25% values",ylab="beta1")
dev.off()


#### starting with different initial values ( part e)

# This part of code takes less than 1 min

set.seed(18)
mySamples1=mhsamplera(n = 80000,dat = data1,tau = 0.67,beta1init = 7.2)$mchain
mySamples2=mhsamplera(n = 80000,dat = data1,tau = 0.67,beta1init = 2)$mchain
mySamples3=mhsamplera(n = 80000,dat = data1,tau = 0.67,beta1init = 20)$mchain


plot1=estvssamp(mySamples1,retval = TRUE)
plot2=estvssamp(mySamples2,retval = TRUE)
plot3=estvssamp(mySamples3,retval = TRUE)


# plotting acf
par(mfrow=c(1,3))

plot(density(mySamples[((length(mySamples)/(4/3)):length(mySamples))]),
     main=" density of posterior function of beta1 after Burn-in Process \n keep last 25% values",ylab="beta1")


acf(t$mchain,main="Autocorrelation Function for Samples of beta1")

# plotting est points with three different starting values
plot(plot1,type='l',ylim = c(7.3,7.420),
     main="M.chains with three different initial values (1,7.2,20)",ylab="beta1",xlab="sample size")
lines(plot2,lty=2,col="red")
lines(plot3,lty=4,col="green",lwd = 3)
dev.off()





############ 
############    Question 2
############


# getting data from the following website

data2= read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")


# producing VMH-sampler

mhsamplerb<-function(n,dat,tau0,tau1,shapel, initval) { 
  # tau0 is sd of proposal dist in random walk metropolis updates for beta0
  # tau1 is sd of proposal dist in random walk metropolis updates for beta1
  # shapel is shape and sclae of proposal dist for lambda which is Gamma (shape,currlambda/shape)
  # initval is initial values for (beta0, beta1, lambda)
  
  Y=dat[,2]   # predictors for Data 1
  X=dat[,1]   # Responses  for Data 2
  
  sigma=1
  
  mchain=matrix(NA,3,n)   # markov chain for beta1
  accb0=0             # acceptance rate for beta0
  accb1=0             # acceptance rate for beta1
  accl=0             # acceptance rate for lambda
  
  # setting initial values
  mchain[,1]=initval
  
  for(i in 2:n){
    
    #### we start to update beta0
    
    currbeta0=mchain[1,i-1]   # value of beta0 in current time
    currbeta1=mchain[2,i-1]   # value of beta1 in current time
   currlambda=mchain[3,i-1]   # value of lambda in current time
    
    ### sample from full canditional distribution of beta0
    # random walk metropolis updates for beta0
    propbeta0=rnorm(n = 1,mean = currbeta0,sd = tau0)

    # producing acceptance ration in log-scale for beta0
    logMHratio.b0=dnorm(x = propbeta0,mean = 0,sd = 10,log = TRUE)+
      sum(dexpgauss(x = Y,mu = propbeta0+currbeta1*X,sigma =  sigma, lambda =  currlambda,log = TRUE))-
      dnorm(x = currbeta0,mean = 0,sd = 10,log = TRUE)-
      sum(dexpgauss(x = Y,mu = currbeta0+currbeta1*X,sigma =  sigma, lambda =  currlambda,log = TRUE))
    
    
    logalpha.b0=min(log(1),logMHratio.b0)  # acceptance probability for beta0
    
   
   
   # accept or reject part
   
    if(log(runif(1))<=logalpha.b0) { # accept with prob. alpha.bo
      accb0=accb0+1 
      mchain[1,i]=propbeta0
    }
    else{   # when  we reject the sample
      mchain[1,i]=currbeta0
    }
   
   
   
   ## we continue to update beta1 
   
   currbeta0=mchain[1,i] # most update should be considered
   
   ### sample from full canditional distribution of beta1
   # random walk metropolis updates for beta1
   
   propbeta1=rnorm(n = 1,mean = currbeta1,sd = tau1)
   
   # producing acceptance ration in log-scale for beta1
   logMHratio.b1=dnorm(x = propbeta1,mean = 0,sd = 10,log = TRUE)+
     sum(dexpgauss(x = Y,mu = currbeta0+propbeta1*X,sigma =  sigma, lambda =  currlambda,log = TRUE))-
     dnorm(x = currbeta1,mean = 0,sd = 10,log = TRUE)-
     sum(dexpgauss(x = Y,mu = currbeta0+currbeta1*X,sigma =  sigma, lambda =  currlambda,log = TRUE))
   
   
   logalpha.b1=min(log(1),logMHratio.b1)  # acceptance probability for beta0
   
   # accept or reject part
   
   if(log(runif(1))<=logalpha.b1) { # accept with prob. alpha.bo
     accb1=accb1+1 
     mchain[2,i]=propbeta1
   }
   else{   # when  we reject the sample
     mchain[2,i]=currbeta1
   }
    
   
   
   ## we continue to update lambda
   
   currbeta1=mchain[2,i] # most update should be considered
   
   ### sample from full canditional distribution of beta1
   # metropolis hastings update
   
   proplambda=rgamma(n = 1,shape = shapel,scale = (currlambda/shapel))
   
   
   # producing acceptance ration in log-scale for lambda
   logMHratio.l=dgamma(x = currlambda,shape = shapel,scale = proplambda/shapel,log = TRUE)+
     dgamma(x = proplambda,shape = 0.01,scale = 100,log = TRUE)+
     sum(dexpgauss(x = Y,mu = currbeta0+currbeta1*X,sigma =  sigma, lambda =  proplambda,log = TRUE))-
     dgamma (x = proplambda,shape = shapel,scale = currlambda/shapel,log = TRUE)-
     dgamma(x = currlambda,shape = 0.01,scale = 100,log = TRUE)-
     sum(dexpgauss(x = Y,mu = currbeta0+currbeta1*X,sigma =  sigma, lambda =  currlambda,log = TRUE))
   
   
   logalpha.l=min(log(1),logMHratio.l)  # acceptance probability for lambda
   
   # accept or reject part
   
   if(log(runif(1))<=logalpha.l) { # accept with prob. alpha.l
     accl=accl+1 
     mchain[3,i]=proplambda
   }
   else{   # when  we reject the sample
     mchain[3,i]=currlambda
   }
  
  }
  
  #return(mchain)
  return(list(acc.r.beta0=accb0/n,acc.r.beta1=accb1/n,acc.r.lambda=accl/n,mchain=mchain))
  
}

set.seed(18)
# it takes less than 4 minutes
tb=mhsamplerb(n=160000,dat=data2,tau0=0.15,tau1=0.25,shapel=50, initval=c(2.4,3.4,0.8))

# finding acceptance rates forbeta0, beta1 and lambda

tb$acc.r.beta0
tb$acc.r.beta1
tb$acc.r.lambda


# for finding better initial values

tail(tb$mchain[1,])
tail(tb$mchain[2,])
tail(tb$mchain[3,])




#### part b

# finding point estimates for posterior means and MCMC s.errors 

bm(tb$mchain[1,])
bm(tb$mchain[2,])
bm(tb$mchain[3,])

# finding ess for beta0, beta1 and lambda

ess(tb$mchain[1,])
ess(tb$mchain[2,])
ess(tb$mchain[3,])

# 95% credible intervals for beta0, beta1 and lambda
quantile(tb$mchain[1,],c(0.025,0.975))   # 95% credible interval based on samples
quantile(tb$mchain[2,],c(0.025,0.975))   # 95% credible interval based on samples
quantile(tb$mchain[3,],c(0.025,0.975))   # 95% credible interval based on samples


# (part c)
# finding correlation between beta0 and beta1

cor(tb$mchain[1,],tb$mchain[2,])

# # Even after Burn.in Process they remain highly correlated
# cor(tb$mchain[1,((length(tb$mchain[1,])/2):length(tb$mchain[1,]))],tb$mchain[2,((length(tb$mchain[2,])/2):length(tb$mchain[2,]))])



# (part d)
# ploting marginal density functions for beta0, beta1 and lambda

# par(mfrow=c(2,3))
# plot(density(tb$mchain[1,]),main=" density for posterior function ( beta1 )",ylab="density beta0")
# plot(density(tb$mchain[2,]),main=" density for posterior function ( beta1 )",ylab="density beta1")
# plot(density(tb$mchain[3,]),main=" density for posterior function ( beta1 )",ylab="density lambda")

#### plot densities after a "Burn-in" process- keeping last 25% values

par(mfrow=c(1,3))
plot(density(tb$mchain[1,((length(tb$mchain[1,])/(4/3)):length(tb$mchain[1,]))]),ylab = "dinsity beta0",
     main=" density for posterior function ( beta1 ) after Burn.in  process \n keeping last 25% values " )
plot(density(tb$mchain[2,((length(tb$mchain[2,])/(4/3)):length(tb$mchain[2,]))]),ylab="density beta1",
     main=" density for posterior function ( beta1 ) after Burn.in  process  \n keeping last 25% values")
plot(density(tb$mchain[3,((length(tb$mchain[3,])/(4/3)):length(tb$mchain[3,]))]),ylab="density lambda",
     main=" density for posterior function ( beta1 ) after Burn.in  process  \n keeping last 25% values ")

dev.off()




##### checking with three different initial values   (part e)


#### starting with different initial values

# this part of codes takes less than 2 minutes

set.seed(18)
mySamples1=mhsamplerb(n=40000,dat=data2,tau0=0.15,tau1=0.25,shapel=50, initval=c(2.4,3.4,0.8))$mchain
mySamples2=mhsamplerb(n=40000,dat=data2,tau0=0.15,tau1=0.25,shapel=50, initval=c(7.5,6,2))$mchain
mySamples3=mhsamplerb(n=40000,dat=data2,tau0=0.15,tau1=0.25,shapel=50, initval=c(-1,-1.4,0.2))$mchain


# different plots for beta0 when we run three different initial values
plot1.0=estvssamp(mySamples1[1,],retval = TRUE)
plot2.0=estvssamp(mySamples2[1,],retval = TRUE)
plot3.0=estvssamp(mySamples3[1,],retval = TRUE)

# different plots for beta1 when we run three different initial values
plot1.1=estvssamp(mySamples1[2,],retval = TRUE)
plot2.1=estvssamp(mySamples2[2,],retval = TRUE)
plot3.1=estvssamp(mySamples3[2,],retval = TRUE)

# different plots for lambda when we run three different initial values
plot1.l=estvssamp(mySamples1[3,],retval = TRUE)
plot2.l=estvssamp(mySamples2[3,],retval = TRUE)
plot3.l=estvssamp(mySamples3[3,],retval = TRUE)


## autocorrelation functions for beta0, beta1 and lambda
par(mfrow=c(2,3))
acf(tb$mchain[1,],main="Autocorrelation Function for Samples of beta0")
acf(tb$mchain[2,],main="Autocorrelation Function for Samples of beta1")
acf(tb$mchain[3,],main="Autocorrelation Function for Samples of lambda")


# # Even after Burn.in process ther are heavily correlated
# par(mfrow=c(1,3))
# acf(tb$mchain[1,((length(tb$mchain[1,])/2):length(tb$mchain[1,]))])
# acf(tb$mchain[2,((length(tb$mchain[1,])/2):length(tb$mchain[1,]))])
# acf(tb$mchain[3,((length(tb$mchain[1,])/2):length(tb$mchain[1,]))])
# dev.off()


plot(plot1.0,type='l',ylim=c(2.25,2.45),main="M.chains with three different initial values for beta0 vs s.size=40000",
     sub="initial values (beta0,beta1,lambda)=     (2.4,3.4,0.8),     (7.5,6,2),     (-1,-1.4,0.2)")
lines(plot2.0,lty=2,col="red")
lines(plot3.0,lty=4,col="green",lwd = 3)

# put the plots together for beta1.
plot(plot1.1,type='l',ylim=c(3.3,3.5),main="M.chains with three different initial values for beta1 vs s.size=40000",
     sub="initial values (beta0,beta1,lambda)=     (2.4,3.4,0.8),     (7.5,6,2),     (-1,-1.4,0.2)")
lines(plot2.1,lty=2,col="red")
lines(plot3.1,lty=4,col="green",lwd = 3)



# put the plots together for lambda
plot(plot1.l,type='l',ylim=c(0.72,0.95),main="M.chains with three different initial values for lambda vs s.size=40000",
     sub="initial values (beta0,beta1,lambda)=     (2.4,3.4,0.8),     (7.5,6,2),     (-1,-1.4,0.2)")
lines(plot2.l,lty=2,col="red")
lines(plot3.l,lty=4,col="green",lwd = 3)


dev.off()



############ 
############    Question 3
############

# getting data from the following website

library(MASS)
data3= read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")



set.seed(18)
tc=mhsamplerb(n=10000,dat=data3,tau0=0.12,tau1=0.25,shapel=100, initval=c(0.1,2.3,0.16))


# finding acceptance rates for beta0, beta1 and lambda
tc$acc.r.beta0
tc$acc.r.beta1
tc$acc.r.lambda



# for finding better initial values.
# put last values as initial values of new sampler
tail(tc$mchain[1,])
tail(tc$mchain[2,])
tail(tc$mchain[3,])


# checking auto correlation functions
# same as Question 2, shows heavily autocorrelation
# However, this is a bit better than Question number 2


acf(tc$mchain[1,],main="acf for beta0")
acf(tc$mchain[2,],main="acf for beta1")
acf(tc$mchain[3,],main="acf for lambda")



# finding point estimate of mean and MCMCs.errors of beta0, beta1 and lambda
bm(tc$mchain[1,])
bm(tc$mchain[2,])
bm(tc$mchain[3,])

# This part is not reported in the writeup and I use the modify version of my model in part c
# # ess for beta0, beta1 and lambda
# ess(tc$mchain[1,])
# ess(tc$mchain[2,])
# ess(tc$mchain[3,])

# finding 95% credible intervals for beta0, beta1 and lambda
quantile(tc$mchain[1,],c(0.025,0.975))   # 95% credible interval based on samples
quantile(tc$mchain[2,],c(0.025,0.975))   # 95% credible interval based on samples
quantile(tc$mchain[3,],c(0.025,0.975))   # 95% credible interval based on samples


# (part b)
# ploting marginal density functions for beta0, beta1 and lambda

par(mfrow=c(2,3))
plot(density(tc$mchain[1,]),main=" density for posterior function ( beta1 )")
plot(density(tc$mchain[2,]),main=" density for posterior function ( beta1 )")
plot(density(tc$mchain[3,]),main=" density for posterior function ( beta1 )")

#### plot densities after a "Burn-in" process

plot(density(tc$mchain[1,((length(tc$mchain[1,])/3):length(tc$mchain[1,]))]),main=" density for posterior function ( beta1 ) after Burn.in  process"  )
plot(density(tc$mchain[2,((length(tc$mchain[2,])/3):length(tc$mchain[2,]))]),main=" density for posterior function ( beta1 ) after Burn.in  process ")
plot(density(tc$mchain[3,((length(tc$mchain[3,])/3):length(tc$mchain[3,]))]),main=" density for posterior function ( beta1 ) after Burn.in  process ")

dev.off()

# finding correlation and covariance between beta0 and beta1
# we show that they are hevily correlated same as question  number 2

cor(tc$mchain[1,],tc$mchain[2,])
cov(tc$mchain[1,],tc$mchain[2,])


### part c
# as beta0 and beta1 are highly correlated I want to take take them as a block
# I modify the previous code till works for this approach.

modeifymhsamplerb<-function(n,dat,tau0,tau1,cov, shapel, initval) { 
  # cov is covariance matrix for 2-dim normal distribution ( for proposal function )
  # shapel is shape and sclae of proposal dist for lambda which is Gamma (shape,currlambda/shape)
  # initval is initial values for (beta0, beta1, lambda)
  
  Y=dat[,2]   # predictors for Data 1
  X=dat[,1]   # Responses  for Data 2
  
  sigma=1
  
  mchain=matrix(NA,3,n)   # markov chain for beta1
  accb=0             # acceptance rate for block (beta0,beta1)
  accl=0             # acceptance rate for lambda
  
  
  mchain[,1]=initval
  
  for(i in 2:n){
    
    #### we start to update beta
    
    currbeta=c(mchain[1,i-1],mchain[2,i-1])   # value of (beta0,beta1) in current time
    currlambda=mchain[3,i-1]   # value of lambda in current time
    
    ### sample from full canditional distribution of beta=(beta0,beta1)
    # random walk updates for 2-dim blocks
    propbeta=mvrnorm(n = 1,mu = currbeta,Sigma = matrix(c(tau0^2,cov,cov,tau1^2),nrow = 2,ncol = 2,byrow = TRUE))
    
    # producing acceptance ration in log-scale for beta=(beta0,beta1)
    
    logMHratio.b=dnorm(x = propbeta[1],mean = 0,sd = 10,log = TRUE)+
      dnorm(x = propbeta[2],mean = 0,sd = 10,log = TRUE)+
      sum(dexpgauss(x = Y,mu = propbeta[1]+propbeta[2]*X,sigma =  sigma, lambda =  currlambda,log = TRUE))-
      dnorm(x = currbeta[1],mean = 0,sd = 10,log = TRUE)-
      dnorm(x = currbeta[2],mean = 0,sd = 10,log = TRUE)-
      sum(dexpgauss(x = Y,mu = currbeta[1]+currbeta[2]*X,sigma =  sigma, lambda =  currlambda,log = TRUE))
    
    
    logalpha.b=min(log(1),logMHratio.b)  # acceptance probability for beta=(beta0,beta1)
    
    # accept or reject part
    
    if(log(runif(1))<=logalpha.b) { # accept with prob. alpha.bo
      accb=accb+1 
      mchain[1,i]=propbeta[1]
      mchain[2,i]=propbeta[2]
    }
    else{   # when  we reject the sample
      mchain[1,i]=currbeta[1]
      mchain[2,i]=currbeta[2]
    }
    
    
    ## we continue to update lambda
    
    currbeta=c(mchain[1,i],mchain[2,i])
    
    ### sample from full canditional distribution of beta1
    # Using generalmetropolis hastings updates
    proplambda=rgamma(n = 1,shape = shapel,scale = (currlambda/shapel))
    
    # producing acceptance ration in log-scale for lambda
    
    logMHratio.l=dgamma(x = currlambda,shape = shapel,scale = proplambda/shapel,log = TRUE)+
      dgamma(x = proplambda,shape = 0.01,scale = 100,log = TRUE)+
      sum(dexpgauss(x = Y,mu = currbeta[1]+currbeta[2]*X,sigma =  sigma, lambda =  proplambda,log = TRUE))-
      dgamma (x = proplambda,shape = shapel,scale = currlambda/shapel,log = TRUE)-
      dgamma(x = currlambda,shape = 0.01,scale = 100,log = TRUE)-
      sum(dexpgauss(x = Y,mu = currbeta[1]+currbeta[2]*X,sigma =  sigma, lambda =  currlambda,log = TRUE))
    
    
    logalpha.l=min(log(1),logMHratio.l)  # acceptance probability for lambda
    
    
    # accept or reject part
    
    if(log(runif(1))<=logalpha.l) { # accept with prob. alpha.l
      accl=accl+1 
      mchain[3,i]=proplambda
    }
    else{   # when  we reject the sample
      mchain[3,i]=currlambda
    }
    
  }
  
  #return(mchain)
  return(list(acc.r.beta=accb/n,acc.r.lambda=accl/n,mchain=mchain))
  
}
# This takes less than 3 minutes
set.seed(18)
mtc=modeifymhsamplerb(n=80000,dat=data3,tau0=0.15,tau1=0.25,cov=-0.03,shapel=100, initval=c(-0.05,3.13,0.16))

mtc$acc.r.beta
mtc$acc.r.lambda


# for finding better initial values
tail(mtc$mchain[1,])
tail(mtc$mchain[2,])
tail(mtc$mchain[3,])

# checking autocorrelation functions
# compaer with previous one
# we see that blocking can leave really positive effects on autocorrelation
par(mfrow=c(2,3))

acf(tc$mchain[1,],main="acf for beta0")
acf(tc$mchain[2,],main="acf for beta1")
acf(tc$mchain[3,],main="acf for lambda")

acf(mtc$mchain[1,],main="acf for beta0 after blocking")
acf(mtc$mchain[2,],main="acf for beta1 after blocking")
acf(mtc$mchain[3,],main="acf for lambda after blocking")


dev.off()

# (part a)
# finding point of estimates of mean and MCMCs.errors for beta0, beta1 and lambda after blocking

bm(mtc$mchain[1,])
bm(mtc$mchain[2,])
bm(mtc$mchain[3,])


# ess for beta0, beta1 and lambda after bloking
ess(mtc$mchain[1,])
ess(mtc$mchain[2,])
ess(mtc$mchain[3,])

# 95% credible intervals forbeta0, beta1 and lambda after blocking
quantile(mtc$mchain[1,],c(0.025,0.975))   # 95% credible interval based on samples
quantile(mtc$mchain[2,],c(0.025,0.975))   # 95% credible interval based on samples
quantile(mtc$mchain[3,],c(0.025,0.975))   # 95% credible interval based on samples




# ploting marginal density functions for beta0, beta1 and lambda after blocking

# par(mfrow=c(2,3))
# plot(density(mtc$mchain[1,]),main=" density for posterior function ( beta1 ) after blocking")
# plot(density(mtc$mchain[2,]),main=" density for posterior function ( beta1 ) after blocking")
# plot(density(mtc$mchain[3,]),main=" density for posterior function ( beta1 ) after blocking")

#### plot densities after a "Burn-in" process

par(mfrow=c(1,3))
plot(density(mtc$mchain[1,((length(mtc$mchain[1,])/3):length(mtc$mchain[1,]))]),main=" density for posterior function ( beta1 ) after blocking and Burn.in   process"  )
plot(density(mtc$mchain[2,((length(mtc$mchain[2,])/3):length(mtc$mchain[2,]))]),main=" density for posterior function ( beta1 ) after bloking and Burn.in  process ")
plot(density(mtc$mchain[3,((length(mtc$mchain[3,])/3):length(mtc$mchain[3,]))]),main=" density for posterior function ( beta1 ) after blocking Burn.in  process ")

dev.off()


# correlation between beta0 and beta1 after blocking
#cor(mtc$mchain[1,],mtc$mchain[2,])

