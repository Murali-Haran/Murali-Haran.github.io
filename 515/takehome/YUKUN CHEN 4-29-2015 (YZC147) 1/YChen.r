##############################
#Final of STAT 515           #
#Yukun Chen                  #         
##############################

#Problem 1####################
#a)       ####################
source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R')
## read in data
xsys=read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat');
xs=xsys[,1];
ys=xsys[,2];
## initial guess for beta1
beta1guess=8.5; # Set according to several runs of MH sampling and plot of the density of beta1
## some parameters
beta0=5;
lambda=0.4;
sigma=1;
set.seed(10)
mhsampler=function(NUMIT=10000,X=xs,Y=ys){
  n=length(X);
  cat("n=",n,"\n");
  mchain = matrix(NA, 1, NUMIT);
  acc=0; # count number of accepted proposals (for beta_1 only)
  mchain[,1]=beta1guess;
  for(i in 2:NUMIT){
    if(i%%100==0){
      cat("Progress: ",i/NUMIT*100,"%\n");
    }
    currbeta1=mchain[1,i-1];
    propbeta1=currbeta1+rnorm(1,mean=0,sd=1);
    ## Metropolis accept-reject step (in log scale)
    logMHratio=sum(dexpgauss(Y,beta0+propbeta1*X,sigma,lambda,TRUE))+pnorm(propbeta1,0,10,log.p=TRUE)-sum(dexpgauss(Y,beta0+currbeta1*X,sigma,lambda,TRUE))-pnorm(currbeta1,0,10,log.p=TRUE)
    #logMHratio=sum(mapply(function(ys,xs){return(dexpgauss(ys,beta0+propbeta1*xs,sigma,lambda,TRUE))},Y,X))+pnorm(propbeta1,0,10,log.p=TRUE)-sum(mapply(function(ys,xs){return(dexpgauss(ys,beta0+currbeta1*xs,sigma,lambda,TRUE))},Y,X))-pnorm(currbeta1,0,10,log.p=TRUE)
    logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
    if(log(runif(1))<logalpha){
      acc = acc + 1 # increment count of accepted proposals
      currbeta1=propbeta1;
    }
    mchain[,i]=currbeta1;
  }
  cat("Markov chain algorithm ran for ",NUMIT,"iterations (acc.rate for beta1=",acc/(NUMIT-1),")\n")
  cat("Parameters is beta1\n")
  return(mchain)
}
betasample=mhsampler(100000,xs,ys);
betasample=t(betasample)
#b)       ####################
source('http://sites.stat.psu.edu/~mharan/batchmeans.R')
beta1_mc=bm(betasample)
E_beta1=beta1_mc$est #7.353536
mcse_beta1=beta1_mc$se # 0.002160992
#c)       ####################
quantile(betasample, c(0.025, 0.975)) #6.735108 7.948522 
#d)       ####################
op = par(mfrow=c(1,1),family="serif")
plot(density(betasample),xlim=range(betasample),main="density of beta1")
par(op)
#e)       ####################
## do MH from different initial values.
acf(betasample)
cat("########## The effective sample size for beta1 is    #############.")
ess(betasample);
beta1guess=12
betasample2=mhsampler(100000,xs,ys);
betasample2=t(betasample2)
beta1guess=4
betasample3=mhsampler(100000,xs,ys);
betasample3=t(betasample3)
## Create Graph E(beta_1)~samplesize and MCse(beta_1)~samplesize
samplesize=seq(500,100000,500);
E_beta_1=rep(NA,length(samplesize));
se_beta_1=rep(NA,length(samplesize));
E_beta_1_2=rep(NA,length(samplesize));
se_beta_1_2=rep(NA,length(samplesize));
E_beta_1_3=rep(NA,length(samplesize));
se_beta_1_3=rep(NA,length(samplesize));
for(i in 1:length(samplesize)){
  temp_beta1_mc=bm(betasample[1:samplesize[i]]);
  temp_beta1_mc2=bm(betasample2[1:samplesize[i]]);
  temp_beta1_mc3=bm(betasample3[1:samplesize[i]]);
  E_beta_1[i]=temp_beta1_mc$est;
  se_beta_1[i]=temp_beta1_mc$se;
  E_beta_1_2[i]=temp_beta1_mc2$est;
  se_beta_1_2[i]=temp_beta1_mc2$se;
  E_beta_1_3[i]=temp_beta1_mc3$est;
  se_beta_1_3[i]=temp_beta1_mc3$se;
}
op = par(mfrow=c(2,3),family="serif")
acf(betasample,main="auto correlation when initial is 8.5")
acf(betasample2,main="auto correlation when initial is 12")
acf(betasample3,main="auto correlation when initial is 4")

plot(samplesize,E_beta_1,ylim=range(E_beta_1,E_beta_1_2,E_beta_1_3),type="l",main="Estimation comparison for beta_1",xlab="sample size",ylab=expression(E(beta_1)));
lines(samplesize,E_beta_1_2,type="l",col="red")
lines(samplesize,E_beta_1_3,type="l",col="blue")
legend(x=65000,y=7.4,c("initial:8.5","initial:12","initial:4"), 
       fill=c("black","blue","red"),bty="n")
plot(samplesize,se_beta_1,ylim=range(se_beta_1,se_beta_1_2,se_beta_1_3),type="l",main="MCse comparison for beta_1",xlab="sample size",ylab=expression(MCse(beta_1)));
lines(samplesize,se_beta_1_2,type="l",col="red")
lines(samplesize,se_beta_1_3,type="l",col="blue")
legend(x=65000,y=0.07,c("initial:8.5","initial:12","initial:4"), 
       fill=c("black","blue","red"),bty="n")
par(op)

#Problem 2####################
#a)       ####################
source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R')
## read in data
xsys=read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat');
xs=xsys[,1];
ys=xsys[,2];
## initial guess for beta0, beta1 and lambda,  Set according to several runs of MH sampling and plot of the marginal density of them
beta0guess=2;
beta1guess=3.2; 
lambdaguess=1
## some parameters
sigma=1;
set.seed(10)
mhsampler2=function(NUMIT=10000,X=xs,Y=ys){
  n=length(X);
  cat("n=",n,"\n");
  mchain = matrix(NA, 3, NUMIT);
  acc_beta_0=0; # count number of accepted proposals (for beta_0 only)
  acc_beta_1=0; # count number of accepted proposals (for beta_0 only)
  acc_lambda=0; # count number of accepted proposals (for lambda only)
  mchain[,1]=c(beta0guess,beta1guess,lambdaguess);
  for(i in 2:NUMIT){# print out progress when the sampler is running
    if(i%%100==0){
      cat("Progress: ",i/NUMIT*100,"%\n");
    }
    currbeta0=mchain[1,i-1];
    currbeta1=mchain[2,i-1];
    currlambda=mchain[3,i-1];
    propbeta0=currbeta0+rnorm(1,mean=0,sd=0.25);
    propbeta1=currbeta1+rnorm(1,mean=0,sd=0.25);
    proplambda=currlambda+rnorm(1,mean=0,sd=0.075);
    ## Metropolis accept-reject step (in log scale) for beta0
    logMHratio=sum(dexpgauss(Y,propbeta0+currbeta1*X,sigma,currlambda,TRUE))+pnorm(propbeta0,0,10,log.p=TRUE)-sum(dexpgauss(Y,currbeta0+currbeta1*X,sigma,currlambda,TRUE))-pnorm(currbeta0,0,10,log.p=TRUE);
    logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
    if(log(runif(1))<logalpha){
      acc_beta_0 = acc_beta_0 + 1 # increment count of accepted proposals
      currbeta0=propbeta0;
    }
    ## Metropolis accept-reject step (in log scale) for beta1
    logMHratio=sum(dexpgauss(Y,currbeta0+propbeta1*X,sigma,currlambda,TRUE))+pnorm(propbeta1,0,10,log.p=TRUE)-sum(dexpgauss(Y,currbeta0+currbeta1*X,sigma,currlambda,TRUE))-pnorm(currbeta1,0,10,log.p=TRUE);
    logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
    if(log(runif(1))<logalpha){
      acc_beta_1 = acc_beta_1 + 1 # increment count of accepted proposals
      currbeta1=propbeta1;
    }
    ## Metropolis accept-reject step (in log scale) for lambda
    if(proplambda>0){#if proplambda>0, calculate Acceptance ratio
      logMHratio=sum(dexpgauss(Y,currbeta0+currbeta1*X,sigma,proplambda,TRUE))+pgamma(proplambda,shape=0.01,scale=100,log.p=TRUE)-sum(dexpgauss(Y,currbeta0+currbeta1*X,sigma,currlambda,TRUE))-pgamma(currlambda,shape=0.01,scale=100,log.p=TRUE);                                                                                                             
      logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
    }else{#else stay at current currlambda and rejct the proplambda
      logalpha = -Inf;
    }
    if(log(runif(1))<logalpha){
      acc_lambda = acc_lambda + 1 # increment count of accepted proposals
      currlambda=proplambda;
    }
    mchain[,i]=c(currbeta0,currbeta1,currlambda);
  }
  cat("Markov chain algorithm ran for ",NUMIT,"iterations (acc.rate for beta0=",acc_beta_0/(NUMIT-1),", beta1=",acc_beta_1/(NUMIT-1),", lambda=",acc_lambda/(NUMIT-1),")\n")
  cat("Parameters is beta1\n")
  return(mchain)
}
#b)       ####################
mchain2=mhsampler2(150000,xs,ys);
beta0samples=mchain2[1,];
beta1samples=mchain2[2,];
lambdasamples=mchain2[3,];
source('http://sites.stat.psu.edu/~mharan/batchmeans.R')
beta0_mc=bm(beta0samples)
E_beta0=beta0_mc$est #2.34943
mcse_beta0=beta0_mc$se # 0.002025202
quantile(beta0samples, c(0.025, 0.975)) #2.081625 2.610275 
beta1_mc=bm(beta1samples)
E_beta1=beta1_mc$est #3.462513
mcse_beta1=beta1_mc$se # 0.002935254
quantile(beta1samples, c(0.025, 0.975)) #0.056112 3.869680 
lambda_mc=bm(lambdasamples)
E_lambda=lambda_mc$est #0.8079838
mcse_lambda=lambda_mc$se #  0.0005653991
quantile(lambdasamples, c(0.025, 0.975)) # 0.6978401 0.9332141 
#c)       ####################
cor(beta0samples,beta1samples) #-0.7725857
#d)       ####################
op = par(mfrow=c(1,3),family="serif")
plot(density(beta0samples),xlim=range(beta0samples),main="density of beta0")
plot(density(beta1samples),xlim=range(beta1samples),main="density of beta1")
plot(density(lambdasamples),xlim=range(lambdasamples),main="density of lambda")
par(op)
#d)       ####################
cat("effective sample size for beta0 beta1 and lambda:");
ess(beta0samples) #4636.332
ess(beta1samples) #5353.097
ess(lambdasamples) #11490.77
## acf plot
op = par(mfrow=c(1,3),family="serif")
acf(beta0samples)
acf(beta1samples)
acf(lambdasamples)
par(op)

samplesize=seq(500,150000,500);
E_beta_1=rep(NA,length(samplesize));
se_beta_1=rep(NA,length(samplesize));
E_beta_0=rep(NA,length(samplesize));
se_beta_0=rep(NA,length(samplesize));
E_lambda=rep(NA,length(samplesize));
se_lambda=rep(NA,length(samplesize));
for(i in 1:length(samplesize)){
  temp_beta1_mc=bm(beta1samples[1:samplesize[i]]);
  E_beta_1[i]=temp_beta1_mc$est;
  se_beta_1[i]=temp_beta1_mc$se;
  temp_beta0_mc=bm(beta0samples[1:samplesize[i]]);
  E_beta_0[i]=temp_beta0_mc$est;
  se_beta_0[i]=temp_beta0_mc$se;
  temp_lambda_mc=bm(beta0samples[1:samplesize[i]]);
  E_lambda[i]=temp_lambda_mc$est;
  se_lambda[i]=temp_lambda_mc$se;
}
beta0guess=2.5;
beta1guess=4; 
lambdaguess=2;
cat("generate another Markov Chain with different initial values")
mchain2b=mhsampler2(150000,xs,ys);
beta0samplesb=mchain2b[1,];
beta1samplesb=mchain2b[2,];
lambdasamplesb=mchain2b[3,];
E_beta_1b=rep(NA,length(samplesize));
se_beta_1b=rep(NA,length(samplesize));
E_beta_0b=rep(NA,length(samplesize));
se_beta_0b=rep(NA,length(samplesize));
E_lambdab=rep(NA,length(samplesize));
se_lambdab=rep(NA,length(samplesize));
for(i in 1:length(samplesize)){
  temp_beta1_mcb=bm(beta1samplesb[1:samplesize[i]]);
  E_beta_1b[i]=temp_beta1_mcb$est;
  se_beta_1b[i]=temp_beta1_mcb$se;
  temp_beta0_mcb=bm(beta0samplesb[1:samplesize[i]]);
  E_beta_0b[i]=temp_beta0_mcb$est;
  se_beta_0b[i]=temp_beta0_mcb$se;
  temp_lambda_mcb=bm(beta0samplesb[1:samplesize[i]]);
  E_lambdab[i]=temp_lambda_mcb$est;
  se_lambdab[i]=temp_lambda_mcb$se;
}

op = par(mfrow=c(3,2),family="serif")
plot(samplesize,E_beta_0,ylim=range(E_beta_0,E_beta_0b),type="l",main="Estimation comparison for beta_0",xlab="sample size",ylab=expression(E(beta_0)));
lines(samplesize,E_beta_0b,type="l",col="red");
plot(samplesize,se_beta_0,ylim=range(se_beta_0,se_beta_0b),type="l",main="MCse comparison for beta_0",xlab="sample size",ylab=expression(MCse(beta_0)));
lines(samplesize,se_beta_0b,type="l",col="red")
plot(samplesize,E_beta_1,ylim=range(E_beta_1,E_beta_1b),type="l",main="Estimation comparison for beta_1",xlab="sample size",ylab=expression(E(beta_1)));
lines(samplesize,E_beta_1b,type="l",col="red")
plot(samplesize,se_beta_1,ylim=range(se_beta_1,se_beta_1b),type="l",main="MCse comparison for beta_1",xlab="sample size",ylab=expression(MCse(beta_1)));
lines(samplesize,se_beta_1b,type="l",col="red")
plot(samplesize,E_lambda,ylim=range(E_lambda,E_lambdab),type="l",main="Estimation comparison for lambda",xlab="sample size",ylab=expression(E(lambda)));
lines(samplesize,E_lambdab,type="l",col="red")
plot(samplesize,se_lambda,ylim=range(se_lambda,se_lambdab),type="l",main="MCse comparison for lambda",xlab="sample size",ylab=expression(MCse(lambda)));
lines(samplesize,se_lambdab,type="l",col="red")
par(op)

op = par(mfrow=c(1,3),family="serif")
d1=density(beta0samples);
d2=density(beta0samples[1:50000])
plot(range(d1$x,d2$x),range(d1$y,d2$y),type = "n",main="density of beta0");
lines(d1,col="blue")
lines(d2,col="red")
d3=density(beta1samples);
d4=density(beta1samples[1:50000])
plot(range(d3$x,d4$x),range(d3$y,d4$y),type = "n",main="density of beta1");
lines(d3,col="blue")
lines(d4,col="red")
d5=density(lambdasamples);
d6=density(lambdasamples[1:50000])
plot(range(d5$x,d5$x),range(d5$y,d6$y),type = "n",main="density of lambda");
lines(d5,col="blue")
lines(d6,col="red")
par(op)


#Problem 3####################
#a)       ####################
source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R')
## read in data
xsys=read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat');
xs=xsys[,1];
ys=xsys[,2];
## initial guess for beta0, beta1 and lambda,  Set according to several runs of MH sampling and plot of the marginal density of them
beta0guess=0.2;
beta1guess=2.5; 
lambdaguess=0.2;
## some parameters
sigma=1;
set.seed(10)
mhsampler3=function(NUMIT=10000,X=xs,Y=ys){
  n=length(X);
  cat("n=",n,"\n");
  mchain = matrix(NA, 3, NUMIT);
  acc_beta_0=0; # count number of accepted proposals (for beta_0 only)
  acc_beta_1=0; # count number of accepted proposals (for beta_0 only)
  acc_lambda=0; # count number of accepted proposals (for lambda only)
  mchain[,1]=c(beta0guess,beta1guess,lambdaguess);
  for(i in 2:NUMIT){# print out progress when the sampler is running
    if(i%%100==0){
      cat("Progress: ",i/NUMIT*100,"%\n");
    }
    currbeta0=mchain[1,i-1];
    currbeta1=mchain[2,i-1];
    currlambda=mchain[3,i-1];
    propbeta0=currbeta0+rnorm(1,mean=0,sd=0.25);
    propbeta1=currbeta1+rnorm(1,mean=0,sd=0.25);
    proplambda=currlambda+rnorm(1,mean=0,sd=0.075);
    ## Metropolis accept-reject step (in log scale) for beta0
    if(currlambda<=0){
      cat(i,"  ",currlambda,"\n");
      return(mchain)
    }
    logMHratio=sum(dexpgauss(Y,propbeta0+currbeta1*X,sigma,currlambda,TRUE))+pnorm(propbeta0,0,10,log.p=TRUE)-sum(dexpgauss(Y,currbeta0+currbeta1*X,sigma,currlambda,TRUE))-pnorm(currbeta0,0,10,log.p=TRUE);
    logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
    if(log(runif(1))<logalpha){
      acc_beta_0 = acc_beta_0 + 1 # increment count of accepted proposals
      currbeta0=propbeta0;
    }
    ## Metropolis accept-reject step (in log scale) for beta1
    logMHratio=sum(dexpgauss(Y,currbeta0+propbeta1*X,sigma,currlambda,TRUE))+pnorm(propbeta1,0,10,log.p=TRUE)-sum(dexpgauss(Y,currbeta0+currbeta1*X,sigma,currlambda,TRUE))-pnorm(currbeta1,0,10,log.p=TRUE);
    logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
    if(log(runif(1))<logalpha){
      acc_beta_1 = acc_beta_1 + 1 # increment count of accepted proposals
      currbeta1=propbeta1;
    }
    ## Metropolis accept-reject step (in log scale) for lambda
    if(proplambda>0){#if proplambda>0, calculate Acceptance ratio
      logMHratio=sum(dexpgauss(Y,currbeta0+currbeta1*X,sigma,proplambda,TRUE))+pgamma(proplambda,shape=0.01,scale=100,log.p=TRUE)-sum(dexpgauss(Y,currbeta0+currbeta1*X,sigma,currlambda,TRUE))-pgamma(currlambda,shape=0.01,scale=100,log.p=TRUE);                                                                                                             
      logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
    }else{#else stay at current currlambda and rejct the proplambda
      logalpha = -Inf;
    }
    if(log(runif(1))<logalpha){
      acc_lambda = acc_lambda + 1 # increment count of accepted proposals
      currlambda=proplambda;
    }
    mchain[,i]=c(currbeta0,currbeta1,currlambda);
  }
  cat("Markov chain algorithm ran for ",NUMIT,"iterations (acc.rate for beta0=",acc_beta_0/(NUMIT-1),", beta1=",acc_beta_1/(NUMIT-1),", lambda=",acc_lambda/(NUMIT-1),")\n")
  cat("Parameters is beta1\n")
  return(mchain)
}
#b)       ####################
mchain3=mhsampler3(200000,xs,ys);
beta0samples=mchain3[1,];
beta1samples=mchain3[2,];
lambdasamples=mchain3[3,];
source('http://sites.stat.psu.edu/~mharan/batchmeans.R')
beta0_mc=bm(beta0samples)
E_beta0=beta0_mc$est # 0.1481102
mcse_beta0=beta0_mc$se # 0.002033596
quantile(beta0samples, c(0.025, 0.975)) #-0.1751833  0.4581409 
beta1_mc=bm(beta1samples)
E_beta1=beta1_mc$est #2.478279
mcse_beta1=beta1_mc$se # 0.003348214
quantile(beta1samples, c(0.025, 0.975)) #1.937912 3.023590 
lambda_mc=bm(lambdasamples)
E_lambda=lambda_mc$est #0.1613289
mcse_lambda=lambda_mc$se #  5.560394e-05
quantile(lambdasamples, c(0.025, 0.975)) # 0.1507897 0.1723152 

cor(beta0samples,beta1samples) # -0.8389401
#b)       ####################
op = par(mfrow=c(1,3),family="serif")
plot(density(beta0samples),xlim=range(beta0samples),main="density of beta0")
plot(density(beta1samples),xlim=range(beta1samples),main="density of beta1")
plot(density(lambdasamples),xlim=range(lambdasamples),main="density of lambda")
par(op)
#Other Evaluations      ####################
cat("effective sample size for beta0 beta1 and lambda:");
ess(beta0samples) #6763.383
ess(beta1samples) #6781.89
ess(lambdasamples) #8107.92
## acf plot
op = par(mfrow=c(1,3),family="serif")
acf(beta0samples)
acf(beta1samples)
acf(lambdasamples)
par(op)

samplesize=seq(500,200000,500);
E_beta_1=rep(NA,length(samplesize));
se_beta_1=rep(NA,length(samplesize));
E_beta_0=rep(NA,length(samplesize));
se_beta_0=rep(NA,length(samplesize));
E_lambda=rep(NA,length(samplesize));
se_lambda=rep(NA,length(samplesize));
for(i in 1:length(samplesize)){
  temp_beta1_mc=bm(beta1samples[1:samplesize[i]]);
  E_beta_1[i]=temp_beta1_mc$est;
  se_beta_1[i]=temp_beta1_mc$se;
  temp_beta0_mc=bm(beta0samples[1:samplesize[i]]);
  E_beta_0[i]=temp_beta0_mc$est;
  se_beta_0[i]=temp_beta0_mc$se;
  temp_lambda_mc=bm(beta0samples[1:samplesize[i]]);
  E_lambda[i]=temp_lambda_mc$est;
  se_lambda[i]=temp_lambda_mc$se;
}
beta0guess=2.5;
beta1guess=4; 
lambdaguess=2;
cat("generate another Markov Chain with different initial values")
mchain3b=mhsampler3(200000,xs,ys);
beta0samplesb=mchain3b[1,];
beta1samplesb=mchain3b[2,];
lambdasamplesb=mchain3b[3,];
E_beta_1b=rep(NA,length(samplesize));
se_beta_1b=rep(NA,length(samplesize));
E_beta_0b=rep(NA,length(samplesize));
se_beta_0b=rep(NA,length(samplesize));
E_lambdab=rep(NA,length(samplesize));
se_lambdab=rep(NA,length(samplesize));
for(i in 1:length(samplesize)){
  temp_beta1_mcb=bm(beta1samplesb[1:samplesize[i]]);
  E_beta_1b[i]=temp_beta1_mcb$est;
  se_beta_1b[i]=temp_beta1_mcb$se;
  temp_beta0_mcb=bm(beta0samplesb[1:samplesize[i]]);
  E_beta_0b[i]=temp_beta0_mcb$est;
  se_beta_0b[i]=temp_beta0_mcb$se;
  temp_lambda_mcb=bm(beta0samplesb[1:samplesize[i]]);
  E_lambdab[i]=temp_lambda_mcb$est;
  se_lambdab[i]=temp_lambda_mcb$se;
}

op = par(mfrow=c(3,2),family="serif")
plot(samplesize,E_beta_0,ylim=range(E_beta_0,E_beta_0b),type="l",main="Estimation comparison for beta_0",xlab="sample size",ylab=expression(E(beta_0)));
lines(samplesize,E_beta_0b,type="l",col="red");
plot(samplesize,se_beta_0,ylim=range(se_beta_0,se_beta_0b),type="l",main="MCse comparison for beta_0",xlab="sample size",ylab=expression(MCse(beta_0)));
lines(samplesize,se_beta_0b,type="l",col="red")
plot(samplesize,E_beta_1,ylim=range(E_beta_1,E_beta_1b),type="l",main="Estimation comparison for beta_1",xlab="sample size",ylab=expression(E(beta_1)));
lines(samplesize,E_beta_1b,type="l",col="red")
plot(samplesize,se_beta_1,ylim=range(se_beta_1,se_beta_1b),type="l",main="MCse comparison for beta_1",xlab="sample size",ylab=expression(MCse(beta_1)));
lines(samplesize,se_beta_1b,type="l",col="red")
plot(samplesize,E_lambda,ylim=range(E_lambda,E_lambdab),type="l",main="Estimation comparison for lambda",xlab="sample size",ylab=expression(E(lambda)));
lines(samplesize,E_lambdab,type="l",col="red")
plot(samplesize,se_lambda,ylim=range(se_lambda,se_lambdab),type="l",main="MCse comparison for lambda",xlab="sample size",ylab=expression(MCse(lambda)));
lines(samplesize,se_lambdab,type="l",col="red")
par(op)

op = par(mfrow=c(1,3),family="serif")
d1=density(beta0samples);
d2=density(beta0samples[1:50000])
plot(range(d1$x,d2$x),range(d1$y,d2$y),type = "n",main="density of beta0");
lines(d1,col="blue")
lines(d2,col="red")
d3=density(beta1samples);
d4=density(beta1samples[1:50000])
plot(range(d3$x,d4$x),range(d3$y,d4$y),type = "n",main="density of beta1");
lines(d3,col="blue")
lines(d4,col="red")
d5=density(lambdasamples);
d6=density(lambdasamples[1:50000])
plot(range(d5$x,d5$x),range(d5$y,d6$y),type = "n",main="density of lambda");
lines(d5,col="blue")
lines(d6,col="red")
par(op)

