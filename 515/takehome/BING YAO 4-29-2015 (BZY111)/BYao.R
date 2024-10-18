###
### Stat515 take home exam: first problem
###


data=read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat')
X=data[,1]
Y=data[,2]
source("http://www.stat.psu.edu/%7Emharan/515/hwdir/emg.R")
## Log of joint distribution
log.h<-function(b0,b1,sigma,lambda){
  sum(dexpgauss(Y, mu = b0+b1*X, sigma, lambda, log = TRUE))-b1^2/200
}

## Simulation function which returns samples and acceptance rate
simemg<-function(N,x0,tau){
  b1=rep(NA,N)
  b1[1]=x0  # Initalizing
  rate=0
  for(i in 1:(N-1)){
    y=rnorm(1,b1[i],tau)
    c=log.h(5,y,1,0.4)-log.h(5,b1[i],1,0.4)
    u=runif(1)
    if(u<(min(1,exp(c)))){
      b1[i+1]=y
      rate=rate+1  # Count acceptance number
    }
    else {(b1[i+1]=b1[i])}
  }
  rate=rate/(N-1)
  return(list(b1,rate)) # Return both samples and rate
}

plot(unlist(simemg(10000,0,1)[1]),type='l')

source("http://www.stat.psu.edu/~mharan/batchmeans.R")
N=seq(100,50000,by=1000)
b1est=rep(NA,length(N))  # Estimation sequence
MCse=rep(NA,length(N))  # MCse sequence

##
## Sample means and MC.se vs sample size N
for(i in 1:length(N)){
  samples=unlist(simemg(N[i],0,1)[1])
  b1est[i]=bm(samples)$est
  MCse[i]=bm(samples)$se
}

##
## Acceptance rate vs tuning parameter
tau=seq(0.1,10,by=1)  # Sequence of random walk se (tuning parameter)
rate=rep(NA,length(tau))
for(i in 1:length(tau)){
  rate[i]=simemg(20000,0,tau[i])[2]
}
##
## Plot
par(mfrow=c(2,2))
plot(N,b1est,type='l',main='beta ests vs sample size')
plot(N,MCse,type='l',main='MCse vs sample size')
plot(tau,rate,type='o',main='acp rate vs tau')
##
## Estimation
mysamples=unlist(simemg(40000,0,1)[1])
mean(mysamples)
MCse=bm(mysamples)$se
MCse
acf(mysamples,main='Auto correlation')
ess(mysamples)
plot(density(mysamples),xlim=c(5,9),main='Density plot of beta1')
quantile(mysamples,c(0.025, 0.975))






###
### Stat515 take home exam problem 2 and 3
###

data=read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat')  
# Read EMG2.dat for problem 2, EMG3.dat for problem3
X=data[,1]
Y=data[,2]
source("http://www.stat.psu.edu/%7Emharan/515/hwdir/emg.R")
## Log of the joint distribution
log.h<-function(b0,b1,sigma,lambda){
  a=sum(dexpgauss(Y, mu = b0+b1*X, sigma, lambda, log = TRUE))-
    b0^2/200-b1^2/200-lambda/100+(0.01-1)*log(lambda)
  return(a)
}

## Simulation function to return beta1,beta2,lambda
simemg2<-function(N,x0){
  # Initialize
  b0=rep(NA,N) 
  b0[1]=x0[1]
  b1=rep(NA,N)
  b1[1]=x0[2]
  lambda=rep(NA,N)
  lambda=x0[3]
  rate.b0=0
  rate.b1=0
  rate.lambda=0
  # Variable at a time algorithm
  for(i in 1:(N-1)){
    # Update of beta0
    y1=rnorm(1,b0[i],0.3)
    c1=log.h(y1,b1[i],1,lambda[i])-log.h(b0[i],b1[i],1,lambda[i])
    u1=runif(1)
    if(u1<min(1,exp(c1))){
      b0[i+1]=y1
      rate.b0=rate.b0+1
    }
    else{b0[i+1]=b0[i]}
    # Update of beta1
    y2=rnorm(1,b1[i],0.3)
    c2=log.h(b0[i+1],y2,1,lambda[i])-log.h(b0[i+1],b1[i],1,lambda[i])
    u2=runif(1)
    if(u2<min(1,exp(c2))){
      b1[i+1]=y2
      rate.b1=rate.b1+1
    }
    else{b1[i+1]=b1[i]}
    # Update of lambda
    y3=rexp(1,1/lambda[i])
    c3=log.h(b0[i+1],b1[i+1],1,y3)-log.h(b0[i+1],b1[i+1],1,lambda[i])-y3/lambda[i]+lambda[i]/y3+log(y3/lambda[i])
    u3=runif(1)
    if(u3<min(1,exp(c3))){
      lambda[i+1]=y3
      rate.lambda=rate.lambda+1
    }
    else{lambda[i+1]=lambda[i]}
  }
  rate.b0=rate.b0/(N-1)
  rate.b1=rate.b1/(N-1)
  rate.lambda=rate.lambda/(N-1)
  return(list(b0=b0,b1=b1,lambda=lambda,
              rate.b0=rate.b0,rate.b1=rate.b1,rate.lambda=rate.lambda))
}

x0=c(1,1,1) # Inital vector x0=(beta0.0,beta1.0,lambda.0)
samples=simemg2(270000,x0)
b0=samples$b0
b1=samples$b1
lambda=samples$lambda
# Plot of samples
par(mfrow=c(2,1))
plot(density(b0),main='Density plot of beta0 and beta1')
points(density(b1),type='l',col='red')
legend(0.7,2.2,c('beta0','beta1'),lty=c(1,1),lwd=c(1.0,1.0),col=c("black","red"))
plot(density(lambda),xlim=c(0.12,0.2),main='Density plot of lambda')
# Ess of samples
source("http://www.stat.psu.edu/~mharan/batchmeans.R")
ess(b0)
ess(b1)
ess(lambda)
# Auto correlation
par(mfrow=c(3,1))
acf(b0)
acf(b1)
acf(lambda)
# Acceptance rate
samples$rate.b0
samples$rate.b1
samples$rate.lambda
# Estimation
mean(b0)
mean(b1)
mean(lambda)
bm(b0)$se
bm(b1)$se
bm(lambda)$se
quantile(b0,c(0.025, 0.975))
quantile(b1,c(0.025, 0.975))
quantile(lambda,c(0.025, 0.975))
cor(b0,b1)

## Plots of Samples and MC.se vs sample size 
N=seq(1000,10000,by=200)
est.b0=rep(NA,length(N))  # Estimation sequence
MCse.b0=rep(NA,length(N))  # MCse sequence
est.b1=rep(NA,length(N))  # Estimation sequence
MCse.b1=rep(NA,length(N))  # MCse sequence
est.lambda=rep(NA,length(N))  # Estimation sequence
MCse.lambda=rep(NA,length(N))  # MCse sequence
for(i in 1:length(N)){
  samples=simemg2(N[i],x0)
  est.b0[i]=bm(samples$b0)$est
  MCse.b0[i]=bm(samples$b0)$se
  est.b1[i]=bm(samples$b1)$est
  MCse.b1[i]=bm(samples$b1)$se
  est.lambda[i]=bm(samples$b1)$est
  MCse.lambda[i]=bm(samples$b1)$se
}
par(mfrow=c(3,2))
plot(N,est.b0,type='l',main='est.b0 vs N')
plot(N,MCse.b0,type='l',main='MCse of b0 vs N')
plot(N,est.b1,type='l',main='est.b1 vs N')
plot(N,MCse.b1,type='l',main='MCse of b1 vs N')
plot(N,est.lambda,type='l',main='est.lambda vs N')
plot(N,MCse.lambda,type='l',main='MCse of lambda vs N')

