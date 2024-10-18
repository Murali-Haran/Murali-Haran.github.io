

dat=as.matrix(read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat"))

#Problem 1

logerfc = function(x,y,b0,b1,lambda,sigma){
  sum=0
  for(i in 1:length(x))
    {
      sum=sum+(pnorm(((b0+b1*x[i]) + lambda * sigma * sigma - y[i])/sigma, lower.tail = FALSE,
              log.p = TRUE))
  }
  return(sum)
}

mh=function(iters,data)
{
  X=data[,1]
  Y=data[,2]
  n=length(Y)
  b0=5
  lambda=.4
  sigma=1
  chain=rep(NA,iters)
  acc=0
  
  chain[1]=rnorm(1,0,sqrt(10))
  
  for(i in 2:iters)
  {
    currb1=chain[i-1]
    propb1=rnorm(1,currb1,sqrt(1))
    

    logratio=(n*log(lambda/2)+(lambda*(n*b0+propb1*sum(X)))+(n/2)*lambda^2*sigma^2-lambda*sum(Y)+logerfc(X,Y,b0,propb1,lambda,sigma)-(1/20)*propb1^2)-(n*log(lambda/2)+(lambda*(n*b0+currb1*sum(X)))+(n/2)*lambda^2*sigma^2-lambda*sum(Y)+logerfc(X,Y,b0,currb1,lambda,sigma)-(1/20)*currb1^2)
    logalpha=min(0,logratio)
    draw=runif(1)
    
    
    if(log(draw)<logalpha)
    {
      acc=acc+1
      chain[i]=propb1
    }
    
    else {
      chain[i]=currb1
    } 
  }
  cat("Markov chain algorithm ran for ",iters,"iterations (acc.rate for b1=",acc/(iters-1),")\n")
  return(chain)
}

samples=mh(10000,dat)
cat("Mean of sample Beta1 is ",mean(samples))
mean(samples)
sorted=sort(samples)
CI=quantile(sorted,c(.025,.975))
cat("95 percent Confidence Interval is ",CI)

par(mfrow=c(3,5))
plot(density(samples),main="Figure 1-a: b1 Density")
acf(samples,main="Figure 1-b: b1 ACF")
source("http://www.stat.psu.edu/~mharan/batchmeans.R")
g=bm(samples)
cat("MCMC standard error for b1 is ", g$se)


#Problem 2/3

d=as.matrix(read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat"))

data=as.matrix(read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat"))

mh3=function(iters,data)
{
  X=data[,1]
  Y=data[,2]
  n=length(Y)
  sigma=1
  mchain=matrix(NA,iters,3)
  acclambda=0
  accb0=0
  accb1=0
  mchain[1,]=c(5,5,5)
  #Chain columns are in order of lambda, b0, b1
  
  for(i in 2:iters)
  {
    currlambda=mchain[i-1,1]
    currb0=mchain[i-1,2]
    currb1=mchain[i-1,3]
    
    proplambda=abs(rnorm(1,currlambda,sqrt(1))) #proposal distribution for lambda
    logratiolambda=(n*log(proplambda/2)+proplambda*(n*currb0+currb1*sum(X))+(n/2)*proplambda^2*sigma^2-proplambda*n*mean(Y)+logerfc(X,Y,currb0,currb1,proplambda,sigma)-.99*log(proplambda)-.01*proplambda)-(n*log(currlambda/2)+currlambda*(n*currb0+currb1*sum(X))+(n/2)*currlambda^2*sigma^2-currlambda*n*mean(Y)+logerfc(X,Y,currb0,currb1,currlambda,sigma)-.99*log(currlambda)-.01*currlambda)
    logalpha=min(0,logratiolambda) 
    draw=runif(1)
    
    if(log(draw)<logalpha)
    {
      acclambda=acclambda+1
      currlambda=proplambda
    }
    
    propb0=rnorm(1,currb0,sqrt(1))
    logratiob0=(currlambda*(n*propb0+currb1*sum(X))+logerfc(X,Y,propb0,currb1,currlambda,sigma)-(1/20)*propb0^2)-(currlambda*(n*currb0+currb1*sum(X))+logerfc(X,Y,currb0,currb1,currlambda,sigma)-(1/20)*currb0^2)
    logalphab0=min(0,logratiob0)
    draw=runif(1)
    
    if(log(draw)<logalphab0)
    {
      accb0=accb0+1
      currb0=propb0
    }
    
    propb1=rnorm(1,currb1,sqrt(1))
    logratiob1=(currlambda*(n*currb0+propb1*sum(X))+logerfc(X,Y,currb0,propb1,currlambda,sigma)-(1/20)*propb1^2)-(currlambda*(n*currb0+currb1*sum(X))+logerfc(X,Y,currb0,currb1,currlambda,sigma)-(1/20)*currb1^2)
    logalphab1=min(0,logratiob1)
    draw=runif(1)
    
    if(log(draw)<logalphab1)
    {
      accb1=accb1+1
      currb1=propb1
    }
    
    mchain[i,]=c(currlambda,currb0,currb1)
  }
  cat("Markov chain algorithm ran for ",iters,"iterations (acc.rate for lambda=",acclambda/(iters-1),", acc.rate for b0=",accb0/(iters-1), ", acc.rate for b1=", accb1/(iters-1),")\n")
  return(mchain)
}

f=mh3(10000,d)
lambda1=f[,1]
b01=f[,2]
b11=f[,3]

plot(density(lambda1),main="Figure 2a: Lambda Density")
plot(density(b01),main="Figure 2b: b0 Density")
plot(density(b11),main="Figure 2c: b1 Density")
acf(lambda1,main="Figure 2d- Lambda ACF")
acf(b01,main="Figure 2e- b0 ACF")
acf(b11,main="Figure 2f- b1 ACF")

z=mh3(10000,data)
lambda=z[,1]
b0=z[,2]
b1=z[,3]

plot(density(lambda),main="Figure 3a: Lambda Density")
plot(density(b0),main="Figure 3b: b0 Density")
plot(density(b1),main="Figure 3c: b1 Density")
acf(lambda,main="Figure 3d: Lambda ACF")
acf(b0,main="Figure 3e: b0 ACF")
acf(b1,main="Figure 3f: b1 ACF")
