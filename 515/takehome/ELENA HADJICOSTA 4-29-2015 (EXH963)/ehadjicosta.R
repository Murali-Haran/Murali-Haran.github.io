#exercise 1

#read data
dat1=read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")

#x is the vector with the predictors
#y is the vector with the responses
x=dat1$V1
y=dat1$V2

source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")

#define a function for metropolis algorithm
## n is the number of samples
## start is the initial value of beta1
## variance is the variance of the normal(proposal) distribution
metropolis1=function(n,start,variance)
{beta1=rep(NA,n)
 accept<-0
 beta1[1]<-start
 for (i in 2:n)
  {currbeta1<-beta1[i-1]
  #proposal distribution is N(currbeta1,variance)
  propbeta1<-rnorm(1,mean=currbeta1,sd=sqrt(variance))
  #accept-reject step (in log scale)
  logMHratio<-sum(dexpgauss(y,mu=5+propbeta1*x,sigma=1,lambda=0.4,log=TRUE))-(1/200)*(propbeta1^2)-sum(dexpgauss(y,mu=5+currbeta1*x,sigma=1,lambda=0.4,log=TRUE))+(1/200)*(currbeta1^2)
  logalpha = min(0,logMHratio)
  if (log(runif(1))<logalpha) 
  {accept<-accept+1
   currbeta1<-propbeta1}
  
   #set the new value to beta1
   beta1[i]<-currbeta1}
 
 cat("Markov chain algorithm run for ",n,"iterations (acceptance rate=",accept/(n-1),")\n")
 return(beta1)}

#start value for beta1 is 1 (arbitrary value) and variance for proposal is 0.5

#plot MC estimate vs sample size and MCse vs sample size

source("http://sites.stat.psu.edu/~mharan/MCMCtut/batchmeans.R")

#generate a sequence for sample size
sample.size=seq(100,5000,by=100)

expectation=rep(NA,length(sample.size))
std=rep(NA,length(sample.size))
#draw samples and calculate the expectation and the mcse
for (i in 1:length(sample.size))
{mysample<-metropolis1(sample.size[i],1,0.5)
 expectation[i]<-mean(mysample)
 std[i]<-bm(mysample)$se}


#plots
par(mfrow=c(1,2))
plot(sample.size,expectation,type='l',main="Estimate vs sample size",xlab="Sample size",ylab="Posterion mean for beta1")
plot(sample.size,std,type='l',main="MCse vs sample size",xlab="Sample size",ylab="MCse")

#plot smoothed density
par(mfrow=c(1,1))
plot(density(metropolis1(10000,1,0.5)),main="Smoothed density plot for beta1")

#overlay plots of pdfs for n=10000 and n=5000--to check the algorithm
plot(density(metropolis1(10000,1,0.5)),main="Smoothed density plot for beta1")
lines(density(metropolis1(5000,1,0.5)))

#check the autocorrelation
mysample<-metropolis1(10000,1,0.5)
acf(mysample,main="Autocorrelation")
#number of effective samples
ess(mysample)

#results
mysample<-metropolis1(100000,1,0.5)
posterior.mean=mean(mysample)
posterior.mean
mcse=bm(mysample)$se
mcse

#95% credible interval
quantile(mysample,c(0.025,0.975))




#exercise 2

#read the data
dat2=read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")

#x is the vector for the predictors
#y is the vector for the responses
x2=dat2$V1
y2=dat2$V2

#intstall the "MASS" package
install.packages("MASS")
library(mvtnorm)

#define a function for metropolis algorithm
## n is the number of samples
## start1 is the initial value of beta0
## start2 is the initial value for beta1
## start3 is the inital value for lambda
## var1 is the marginal variance for beta0(in bivariate normal proposal)
## var2 is the marginal variance for beta1(in bivariate normal proposal)
## correl is the correlation of beta0 and beta0(in bivariate normal proposal)
## var3 is included in the parameters of the gamma proposal
metropolis2=function(n,start1,start2,start3,var1,var2,correl,var3)
{mchain=matrix(NA,3,n)
 accept1<-0
 accept2<-0
 mchain[,1]<-c(start1,start2,start3)
  for (i in 2:n)
  {currbeta0=mchain[1,i-1]
   currbeta1=mchain[2,i-1]
   currlambda=mchain[3,i-1]
   
   #define sigma matrix
   Sigma=matrix(NA,nrow=2,ncol=2)
   
   Sigma[1,1]=var1
   Sigma[2,2]=var2
   Sigma[1,2]=correl*sqrt(var1*var2)
   Sigma[2,1]=correl*sqrt(var1*var2)
   
   #draw from a bivariate normal distribution with mean the current values for beta0 and beta1
   X=rmvnorm(1,mean=c(currbeta0,currbeta1),sigma=Sigma)
   propbeta0<-X[1]
   propbeta1<-X[2]
   #accept-reject algorithm (in log-scale) for (beta0,beta1)
   logMHratio.b0b1<-sum(dexpgauss(y2,mu=propbeta0+propbeta1*x2,sigma=1,lambda=currlambda,log=TRUE))-(1/200)*((propbeta0^2)+(propbeta1^2))-sum(dexpgauss(y2,mu=currbeta0+currbeta1*x2,sigma=1,lambda=currlambda,log=TRUE))+(1/200)*((currbeta0^2)+(currbeta1^2))
   logalpha.b0b1<-min(0,logMHratio.b0b1)
   if (log(runif(1))<logalpha.b0b1) 
   {accept1<-accept1+1
    currbeta0<-propbeta0
    currbeta1<-propbeta1}
   
   #draw from a gamma distributon
   proplambda<-rgamma(1,shape=var3*currlambda,scale=1/var3)
   #accept-reject step (in log scale) for lambda
   logMHratio.l<-sum(dexpgauss(y2,mu=currbeta0+currbeta1*x2,sigma=1,lambda=proplambda,log=TRUE))-0.99*log(proplambda)-(proplambda/100)-sum(dexpgauss(y2,mu=currbeta0+currbeta1*x2,sigma=1,lambda=currlambda,log=TRUE))+0.99*log(currlambda)+(currlambda/100)+(var3*proplambda-1)*log(currlambda)-(currlambda*var3)-(currlambda*var3-1)*log(proplambda)+(proplambda*var3)
   logalpha.l<-min(0,logMHratio.l)
   if (log(runif(1))<logalpha.l) 
   {accept2<-accept2+1
    currlambda<-proplambda}
   
   #update chain with new values
   mchain[,i] = c(currbeta0,currbeta1,currlambda)}
 
 return(mchain)}

#plot MC estimate vs sample size and MCse vs sample size
#for b0,b1,lambda

source("http://sites.stat.psu.edu/~mharan/MCMCtut/batchmeans.R")

#generate sample sizes
sample.size=seq(100,5000,by=100)

expectation.b0=rep(NA,length(sample.size))
expectation.b1=rep(NA,length(sample.size))
expectation.lambda=rep(NA,length(sample.size))
std.b0=rep(NA,length(sample.size))
std.b1=rep(NA,length(sample.size))
std.lambda=rep(NA,length(sample.size))
#draw samples and calculate the expectation and std
for (i in 1:length(sample.size))
{mysample<-metropolis2(sample.size[i],1,1,1,0.05,0.05,0,30)
 expectation.b0[i]<-mean(mysample[1,])
 std.b0[i]<-bm(mysample[1,])$se
 expectation.b1[i]<-mean(mysample[2,])
 std.b1[i]<-bm(mysample[2,])$se
 expectation.lambda[i]<-mean(mysample[3,])
 std.lambda[i]<-bm(mysample[3,])$se}

#plots
par(mfrow=c(3,2))
plot(sample.size,expectation.b0,type='l')
plot(sample.size,std.b0,type='l')
plot(sample.size,expectation.b1,type='l')
plot(sample.size,std.b1,type='l')
plot(sample.size,expectation.lambda,type='l')
plot(sample.size,std.lambda,type='l')

#plots of smoothed densities
par(mfrow=c(1,3))
sample1=metropolis2(10000,1,1,1,0.05,0.05,0,30)
plot(density(sample1[1,]),"Pdf for b0")
plot(density(sample1[2,]),"Pdf for b1")
plot(density(sample1[3,]),"Pdf for lambda")

#check the autocorrelation
par(mfrow=c(1,3))
acf(sample1[1,],main='b0')
acf(sample1[2,],main='b1')
acf(sample1[3,],main='lambda')

#effective sample size
ess(sample1[1,])
ess(sample1[2,])
ess(sample1[3,])

#results
mysample=metropolis2(100000,1,1,1,0.05,0.05,0,30)
mean.b0=mean(mysample[1,])
mean.b0
mcse.b0=bm(mysample[1,])$se
mcse.b0
mean.b1=mean(mysample[2,])
mean.b1
mcse.b1=bm(mysample[2,])$se
mcse.b1
mean.l=mean(mysample[3,])
mean.l
mcse.l=bm(mysample[3,])$se
mcse.l

#95% credible intervals
quantile(mysample[1,],c(0.025,0.975))
quantile(mysample[2,],c(0.025,0.975))
quantile(mysample[3,],c(0.025,0.975))

#correlation between beta0 and beta1
cor(mysample[1,],mysample[2,])



#exercise 3
dat3=read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")

#x is the vector for the predictors
#y is the vector for the responses
x3=dat3$V1
y3=dat3$V2


#define a function for metropolis algorithm
## n is the number of samples
## start1 is the initial value of beta0
## start2 is the initial value for beta1
## start3 is the inital value for lambda
## var1 is the marginal variance for beta0(in bivariate normal proposal)
## var2 is the marginal variance for beta1(in bivariate normal proposal)
## correl is the correlation of beta0 and beta0(in bivariate normal proposal)
## var3 is included in the parameters of the gamma proposal
metropolis3=function(n,start1,start2,start3,var1,var2,correl,var3)
{mchain=matrix(NA,3,n)
 accept1<-0
 accept2<-0
 mchain[,1]<-c(start1,start2,start3)
 for (i in 2:n)
 {currbeta0=mchain[1,i-1]
  currbeta1=mchain[2,i-1]
  currlambda=mchain[3,i-1]
  
  #define sigma matrix
  Sigma=matrix(NA,nrow=2,ncol=2)
  
  Sigma[1,1]=var1
  Sigma[2,2]=var2
  Sigma[1,2]=correl*sqrt(var1*var2)
  Sigma[2,1]=correl*sqrt(var1*var2)
  
  #draw from a bivariate normal distribution with mean the current values for beta0 and beta1
  X=rmvnorm(1,mean=c(currbeta0,currbeta1),sigma=Sigma)
  propbeta0<-X[1]
  propbeta1<-X[2]
  #accept-reject algorithm (in log-scale) for (beta0,beta1)
  logMHratio.b0b1<-sum(dexpgauss(y3,mu=propbeta0+propbeta1*x3,sigma=1,lambda=currlambda,log=TRUE))-(1/200)*((propbeta0^2)+(propbeta1^2))-sum(dexpgauss(y3,mu=currbeta0+currbeta1*x3,sigma=1,lambda=currlambda,log=TRUE))+(1/200)*((currbeta0^2)+(currbeta1^2))
  logalpha.b0b1<-min(0,logMHratio.b0b1)
  if (log(runif(1))<logalpha.b0b1) 
  {accept1<-accept1+1
   currbeta0<-propbeta0
   currbeta1<-propbeta1}
  
  #draw from a gamma distributon
  proplambda<-rgamma(1,shape=var3*currlambda,scale=1/var3)
  #accept-reject step (in log scale) for lambda
  logMHratio.l<-sum(dexpgauss(y3,mu=currbeta0+currbeta1*x3,sigma=1,lambda=proplambda,log=TRUE))-0.99*log(proplambda)-(proplambda/100)-sum(dexpgauss(y3,mu=currbeta0+currbeta1*x3,sigma=1,lambda=currlambda,log=TRUE))+0.99*log(currlambda)+(currlambda/100)+(var3*proplambda-1)*log(currlambda)-(currlambda*var3)-(currlambda*var3-1)*log(proplambda)+(proplambda*var3)
  logalpha.l<-min(0,logMHratio.l)
  if (log(runif(1))<logalpha.l) 
  {accept2<-accept2+1
   currlambda<-proplambda}
  
  #update chain with new values
  mchain[,i] = c(currbeta0,currbeta1,currlambda)}
 
 return(mchain)}

#plot MC estimate vs sample size and MCse vs sample size
#for b0,b1,lambda

source("http://sites.stat.psu.edu/~mharan/MCMCtut/batchmeans.R")

#generate sample sizes
sample.size=seq(100,5000,by=100)

expectation.b0=rep(NA,length(sample.size))
expectation.b1=rep(NA,length(sample.size))
expectation.lambda=rep(NA,length(sample.size))
std.b0=rep(NA,length(sample.size))
std.b1=rep(NA,length(sample.size))
std.lambda=rep(NA,length(sample.size))
#draw samples and calculate the expectation and std
for (i in 1:length(sample.size))
{mysample<-metropolis3(sample.size[i],1,1,1,0.1,0.1,0,30)
 expectation.b0[i]<-mean(mysample[1,])
 std.b0[i]<-bm(mysample[1,])$se
 expectation.b1[i]<-mean(mysample[2,])
 std.b1[i]<-bm(mysample[2,])$se
 expectation.lambda[i]<-mean(mysample[3,])
 std.lambda[i]<-bm(mysample[3,])$se}

#plots
par(mfrow=c(3,2))
plot(sample.size,expectation.b0,type='l')
plot(sample.size,std.b0,type='l')
plot(sample.size,expectation.b1,type='l')
plot(sample.size,std.b1,type='l')
plot(sample.size,expectation.lambda,type='l')
plot(sample.size,std.lambda,type='l')

#plots of smoothed densities
par(mfrow=c(1,3))
sample1=metropolis3(10000,1,1,1,0.1,0.1,0,30)
plot(density(sample1[1,]),"Pdf for b0")
plot(density(sample1[2,]),"Pdf for b1")
plot(density(sample1[3,]),"Pdf for lambda")

#check the autocorrelation
par(mfrow=c(1,3))
acf(sample1[1,])
acf(sample1[2,])
acf(sample1[3,])

#effective sample size
ess(sample1[1,])
ess(sample1[2,])
ess(sample1[3,])

#results
mysample=metropolis3(100000,1,1,1,0.1,0.1,0,30)
mean.b0=mean(mysample[1,])
mean.b0
mcse.b0=bm(mysample[1,])$se
mcse.b0
mean.b1=mean(mysample[2,])
mean.b1
mcse.b1=bm(mysample[2,])$se
mcse.b1
mean.l=mean(mysample[3,])
mean.l
mcse.l=bm(mysample[3,])$se
mcse.l

#95% credible intervals
quantile(mysample[1,],c(0.025,0.975))
quantile(mysample[2,],c(0.025,0.975))
quantile(mysample[3,],c(0.025,0.975))

