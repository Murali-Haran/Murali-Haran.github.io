#beware, the code will take a very long time to run
#loading file given by prof. Haran
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")
source("http://sites.stat.psu.edu/~mharan/batchmeans.R")
#loading data
dat1=as.matrix(read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat"))
dat2=as.matrix(read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat"))
dat3=as.matrix(read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat"))
#setting seed
set.seed(21)

#defining function to evaluate log of emg density
ll<-function(x,beta0,beta1,lambda){
    # x: 2 column matrix of data
    # others are parameters at which the
    # log-likelihood is evaluated
    dexpgauss(x[2],beta0+beta1*x[1],1,lambda,log=TRUE)
}

mcmh<-function(n,sd,x1){
    # n: number of runs
    # sd: tuning parameter
    # x1: initial value
    c=0 # counter for acceptances
    X=rep(NA,n) # empty vector to store chain in
    m=n/10
    SE=rep(NA,10)
    X[1]=x1 # setting the initial value
    for (i in 2:n){
        Y=rnorm(1,X[i-1],sd) # proposing Y
        # log of kernel of likelihood at Y
        hy=-Y*Y/200+sum(apply(dat1,1,ll,beta0=5,beta1=Y,lambda=0.4))
        # log of kernel of likelihood at X[i-1]
        hx=-X[i-1]*X[i-1]/200+sum(apply(dat1,1,ll,beta0=5,beta1=X[i-1],lambda=0.4))
        alpha=hy-hx
        if(alpha>log(runif(1,0,1))){
            # if the log of the unif is smaller than alpha we accept
            X[i]=Y
            c=c+1 # increase counter
        }else{
            # otherwise, reject
            X[i]=X[i-1]
        }        
        if(i%%m==0){
            #calculating MCMC se at certain i
            SE[i/m]=bm(X[1:i])$se
        }
    }
    # return the chain with acceptance rate attached at the end
    return(c(X,SE,c/n))
}

mcmh2<-function(n,sd1,sd2,sd3,x11,x12,x13,data){
    # n: number of runs
    # sd*: tuning parameters
    # x1*: initial value1
    # data: the data to be used
    c1=0 # counter for acceptances
    c3=0
    m=n/10 # interval at which to calculate MCMC se
    X1=rep(NA,n) # empty vector to store chains in
    X2=rep(NA,n) 
    X3=rep(NA,n) 
    X1[1]=x11 # setting the initial values
    X2[1]=x12
    X3[1]=x13
    SE1=rep(NA,10) # to store MCMC se
    SE2=rep(NA,10)
    SE3=rep(NA,10)
    for (i in 2:n){
        # similar idea as mcmh, just take care to always use the newest data available
        Y1=rnorm(1,X1[i-1],sd1) # proposing Y1
        Y2=rnorm(1,X2[i-1],sd2) # proposing Y2
        hy=-Y1*Y1/200-Y2*Y2/200+sum(apply(data,1,ll,beta0=Y1,beta1=Y2,lambda=X3[i-1]))
        hx=-X1[i-1]*X1[i-1]/200-X2[i-1]*X2[i-1]/200+sum(apply(data,1,ll,beta0=X1[i-1],beta1=X2[i-1],lambda=X3[i-1]))
        if((hy-hx)>log(runif(1,0,1))){
            X1[i]=Y1
            X2[i]=Y2
            c1=c1+1
        }else{
            X1[i]=X1[i-1]
            X2[i]=X2[i-1]
        }
        Y3=rnorm(1,X3[i-1],sd3) # proposing Y3
        while(Y3<=0){Y3=rnorm(1,X3[i-1],sd3)} # reject the proposal until it is positive
        hy3=-Y3/100-0.99*log(Y3)+sum(apply(data,1,ll,beta0=X1[i],beta1=X2[i],lambda=Y3))+log(pnorm(X3[i-1],sd3))
        # note the last term corresponding to the log of q(y,x)
        hx3=-X3[i-1]/100-0.99*log(X3[i-1])+sum(apply(data,1,ll,beta0=X1[i],beta1=X2[i],lambda=X3[i-1]))+log(pnorm(Y3,sd3))
        # note the last term corresponding to the log of q(x,y)
        if((hy3-hx3)>log(runif(1,0,1))){
            X3[i]=Y3
            c3=c3+1
        }else{
            X3[i]=X3[i-1]
        }
        #calculating MCMC se at certain i
        if(i%%m==0){
            SE1[i/m]=bm(X1[1:i])$se
            SE2[i/m]=bm(X2[1:i])$se
            SE3[i/m]=bm(X3[1:i])$se
        }
    }
    return(c(X1,X2,X3,SE1,SE2,SE3,c1/n,c3/n))
}

#generating chain
X1=mcmh(10000,0.65,7)
X2=mcmh2(10000,0.1,0.15,0.05,2.5,3.3,1,dat2)
X3=mcmh2(10000,0.2,0.2,0.05,0,2.8,0.15,dat3)

#storing them
write(X1,"X1",ncolumns=1)
write(X2,"X2",ncolumns=1)
write(X3,"X3",ncolumns=1)


# here onwards, merely generating plots and data

plot(X1[1:10000],pch=".")
mean(X1[1:10000])
X1[10010]*1.96
quantile(X1[1:10000],c(0.025,0.975))

png("zyuan1.png")
plot(density(X1[1:10000]),main="posterior pdf of beta1")
lines(density(X1[1:5000]),lty=3)
dev.off()
ess(X1[1:10000])


ess(X1[1:10000])

mX1=rep(NA,10000)
for(i in 1:10000){
    mX1[i]=mean(X1[1:i])
}

png("zyuan12.png")
par(mfrow=c(3,1))
acf(X1[1:10000],main="beta1 ACF")
plot(X1[10001:10010],ylab="MCMCM se", xlab="1000's of runs",pch="*")
plot(mX1,type="l",ylab="posterior mean",xlab="# of runs")
dev.off()

mean(X2[1:10000])
mean(X2[10001:20000])
mean(X2[20001:30000])
X2[30010]*1.96
X2[30020]*1.96
X2[30030]*1.96
quantile(X2[1:10000],c(0.025,0.975))
quantile(X2[10001:20000],c(0.025,0.975))
quantile(X2[20001:30000],c(0.025,0.975))
cor(X2[1:10000],X2[10001:20000])

png("zyuan3.png")
plot(density(X2[1:10000]),main="posterior marginal pdf of beta0")
lines(density(X2[1:5000]),lty=3)
dev.off()

png("zyuan4.png")
plot(density(X2[10001:15000]),lty=3,main="posterior marginal pdf of beta1")
lines(density(X2[10001:20000]))
dev.off()


png("zyuan5.png")
plot(density(X2[20001:30000]),main="posterior marginal pdf of lambda")
lines(density(X2[20001:25000]),lty=3)
dev.off()

acf(X2[1:10000])
acf(X2[10001:20000])
acf(X2[20001:30000])

png("zyuan21.png")
par(mfrow=c(3,1))
acf(X2[1:10000],main="beta0 ACF")
acf(X2[10001:20000],main="beta1 ACF")
acf(X2[20001:30000],main="lambda ACF")
dev.off()


png("zyuan6.png")
par(mfrow=c(3,1))
plot(X2[30001:30010],pch="*",ylab="MCMCM se", xlab="1000's of runs",main="beta0")
plot(X2[30011:30020],pch="*",ylab="MCMCM se", xlab="1000's of runs",main="beta1")
plot(X2[30021:30030],pch="*",ylab="MCMCM se", xlab="1000's of runs",main="lambda")
dev.off()

mean(X3[1:10000])
mean(X3[10001:20000])
mean(X3[20001:30000])
X3[30010]*1.96
X3[30020]*1.96
X3[30030]*1.96
quantile(X3[1:10000],c(0.025,0.975))
quantile(X3[10001:20000],c(0.025,0.975))
quantile(X3[20001:30000],c(0.025,0.975))

png("zyuan23.png")
plot(density(X3[1:10000]),main="posterior marginal pdf of beta0")
lines(density(X3[1:5000]),lty=3)
dev.off()

png("zyuan24.png")
plot(density(X3[10001:15000]),lty=3,main="posterior marginal pdf of beta1")
lines(density(X3[10001:20000]))
dev.off()


png("zyuan25.png")
plot(density(X3[20001:30000]),main="posterior marginal pdf of lambda")
lines(density(X3[20001:25000]),lty=3)
dev.off()

png("zyuan22.png")
par(mfrow=c(3,1))
acf(X3[1:10000],main="beta0 ACF")
acf(X3[10001:20000],main="beta1 ACF")
acf(X3[20001:30000],main="lambda ACF")
dev.off()

png("zyuan9.png")
par(mfrow=c(3,1))
plot(X3[30001:30010],pch="*",ylab="MCMCM se", xlab="1000's of runs",main="beta0")
plot(X3[30011:30020],pch="*",ylab="MCMCM se", xlab="1000's of runs",main="beta1")
plot(X3[30021:30030],pch="*",ylab="MCMCM se", xlab="1000's of runs",main="lambda")
dev.off()

mX20=rep(NA,10000)
for(i in 1:10000){
    mX20[i]=mean(X2[1:i])
}

Temp=X2[10001:20000]
mX21=rep(NA,10000)
for(i in 1:10000){
    mX21[i]=mean(Temp[1:i])
}

Temp=X2[20001:30000]
mX22=rep(NA,10000)
for(i in 1:10000){
    mX22[i]=mean(Temp[1:i])
}


png("zyuan31.png")
par(mfrow=c(3,1))
plot(mX20,type="l",ylab="posterior mean of beta0",xlab="# of runs")
plot(mX21,type="l",ylab="posterior mean of beta1",xlab="# of runs")
plot(mX22,type="l",ylab="posterior mean of lambda",xlab="# of runs")
dev.off()

mX30=rep(NA,10000)
for(i in 1:10000){
    mX30[i]=mean(X3[1:i])
}

Temp=X3[10001:20000]
mX31=rep(NA,10000)
for(i in 1:10000){
    mX31[i]=mean(Temp[1:i])
}

Temp=X3[20001:30000]
mX32=rep(NA,10000)
for(i in 1:10000){
    mX32[i]=mean(Temp[1:i])
}


png("zyuan32.png")
par(mfrow=c(3,1))
plot(mX30,type="l",ylab="posterior mean of beta0",xlab="# of runs")
plot(mX31,type="l",ylab="posterior mean of beta1",xlab="# of runs")
plot(mX32,type="l",ylab="posterior mean of lambda",xlab="# of runs")
dev.off()


