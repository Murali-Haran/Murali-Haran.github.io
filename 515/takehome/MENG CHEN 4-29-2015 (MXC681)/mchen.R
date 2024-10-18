#Setting working directory, please ignore
#setwd("C:/Users/mxc681/Dropbox/STAT 515/Takehome")

# Sourcing some useful functions: distribution of EMG, bm, ess, etc
source("http://www.stat.psu.edu/~mharan/batchmeans.R")
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")
###############################################################
##### For plotting mean estimates for multiple chains
###############################################################
estvssamp2 = function(samp,lower,upper, g=mean, plotname="mean estimates 
                      (multiple chains)")
{
  for(j in 1:length(samp)){ 
  color<-rainbow(14)[j+4]
  if (length(samp[[j]])<100)
    batchsize = 1
  else
    batchsize = length(samp[[j]])%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp[[j]]),by=batchsize))
  {
    est = c(est, g(samp[[j]][1:i]))
  }
  
  #    plot(seq(batchsize,length(samp),by=batchsize),est,main=paste("M.C. estimates vs. sample size\n"),type="l",xlab="sample size",ylab="MC estimate")
  if(j==1){
    plot(seq(batchsize,length(samp[[j]]),by=batchsize),est,main=plotname,type="l",col=color,xlab="sample size",ylab="MC estimate",ylim=c(lower,upper))
  }else(lines(seq(batchsize,length(samp[[j]]),by=batchsize),est,col=color))
}}
###############################################################
###### For plotting SE estimates (and for multiple chains)
###############################################################
SEplot = function(samp, g=bm, plotname="MCSE")
{
  if (length(samp)<100)
    batchsize = 1
  else
    batchsize = length(samp)%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp),by=batchsize))
  {
    est = c(est, g(samp[1:i])[[2]])
  }
  
  plot(seq(batchsize,length(samp),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MC estimate")
}

SEplotMulti = function(samp, g=bm, plotname="MCSE (multiple chains)")
{ for(j in 1:length(samp)){ 
  color<-rainbow(14)[j+4]
  if (length(samp[[j]])<100)
    batchsize = 1
  else
    batchsize = length(samp[[j]])%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp[[j]]),by=batchsize))
  {
    est = c(est, g(samp[[j]][1:i])[[2]])
  }
  if(j==1){
    plot(seq(batchsize,length(samp[[j]]),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MC estimate")
  }else(lines(seq(batchsize,length(samp[[j]]),by=batchsize),est,col=color))
}}

#####################################################################
###### For plotting correlation estimates
#####################################################################
CorrTracker = function(samp, colnum1,colnum2,g=cor, plotname="correlation estimates")
{
  if (length(samp[,1])<100)
    batchsize = 1
  else
    batchsize = length(samp[,1])%/%100
  
  est = c()
  for (i in seq(batchsize,length(samp[,1]),by=batchsize))
  {
    est = c(est, g(samp[1:i,colnum1],samp[1:i,colnum2]))
  }
  # return(est)
  plot(seq(batchsize,length(samp[,1]),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MC estimate")
}




# Read in data
dat1<-read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
dat2<-read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
dat3<-read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")

###################################################################################
# Log Likelihood Function for b1 given a dataset (Joint distribution with given data)
###################################################################################
Loglikelihood<-function(b0,b1,lambda,sigma,data){
  mu<-b0+b1*data$V1
  single<-dexpgauss(data$V2,mu,sigma=sigma,lambda=lambda,log=TRUE)
  loglikelihood<-sum(single)
  return(loglikelihood)
}
###################
# Log Prior
###################
Priorb1<-function(b1){dnorm(b1,mean=0,sd=10,log=TRUE)}

###################
# Log Posterior
###################
posteriorb1 <- function(b0,b1,lambda,sigma,data){
  return (Loglikelihood(b0,b1,lambda,sigma,data) + Priorb1(b1))
}

#The following code is used to select a proposal function graphically
lambda=.4
sigma=1
b0=5
b1<-seq(5,9,by=.01)
p<-sapply(b1,posteriorb1,b0=5,lambda=.4,sigma=1,data=dat1)
par(mfrow=c(1,1))
plot(b1,exp(p),type="l",ylab="h(b1)",main="Posterior Density up to a Normalizing Constant")
abline(v=7.34)


#################################################################
#After visual speculation, I selected the proposal to be a normal
#distribution with sd 0.5.
#################################################################
proposalfunction <- function(b1current){
  return(rnorm(1,mean = b1current, sd= .5))
}



#################################################
# Metropolis Update
################################################
#bstart: starting value
#niter: sample size
#data: dataset (dat1, 2,or 3)
MH_b1 <- function(bstart, niter,data){
  chain = rep(NA,niter+1)
  chain[1] = bstart
  for (i in 1:niter){
    proposal = proposalfunction(chain[i])
    r<-exp(posteriorb1(b0=5,proposal,lambda=.4,sigma=1,data) - posteriorb1(b0=5,chain[i],lambda=.4,sigma=1,data))
    prob = min(1,r)
    if (runif(1) <= prob){
      chain[i+1] = proposal
    }else{
      chain[i+1] = chain[i]
    }
#  print(c(i,proposal,chain[i],r))
  }
  return(chain)
}

#------Testing if the sampler works with simulated data. Please ignore------
# b1=8
# x<-runif(500,0,1)
# mu<-5+b1*x
# y<-sapply(mu,rexpgauss,n=1,sigma=1,lambda=.4)
# data<-data.frame(cbind(x,y))
# colnames(data) <- c("V1", "V2")
# 
# b1<-seq(6,10,by=.01)
# p<-sapply(b1,posteriorb1,b0=5,lambda=.4,sigma=1,data=data)
# plot(b1,exp(p),type="l",ylab="h(b1)",main="Posterior Density up to a Normalizing Constant",ylim=c(min(exp(p)),max(exp(p))))
# abline(v=8.33)
# 
# runif(1,6,8)
# a<-as.numeric(NA)
# for(i in 1:50){
# test<-MH_b1(6,50000,data)
# a[i]<-bm(test)$est
# print(i)}
# quantile(test,c(.025,.975))
# ess(test)
# plot(1:50001,test,type="l")

set.seed(70791)
#I picked a starting value between 6 and 8, where the density is above 0 from visual inspection
start1<-runif(1,6,8)
chain1<-MH_b1(start1,50000,dat1)
mean(chain1)
#[1] 7.278008
ess(chain1)
# truncated after  11  lags
# [1] 9283.062
bm(chain1)$se
# [1] 0.003025457
quantile(chain1,c(.025,.975))
#      2.5%     97.5% 
#  6.647189 7.863152 
layout(matrix(c(1,2,3,6,4,5,3,6), 2, 4, byrow = TRUE))
estvssamp(chain1)
SEplot(chain1)
acf(chain1,main="Autocorrelation, 
    Sample Size=50000")
################################################################
########### Diagnostics on MCMC Accurary
################################################################
######Convergence 
#Kernal Density Estimate
plot(density(chain1),col="blue",main="Estimate of Posterior Density
     (blue: after 50,000 samples, red: after 25,000 samples)", xlab="b1",cex.main=.8)
lines(density(chain1[1:25000]),col="red")

#Mutiple starts: 10 starting values in the 6 to 8 range  
start_a_bunch<-seq(6,8,by=.2)[-1]
nsample<-50000
chain<-matrix(rep(NA,(nsample+1)*length(start_a_bunch)),ncol=length(start_a_bunch))
for(i in 1:length(start_a_bunch)){
  chain[,i]<-MH_b1(start_a_bunch[i],nsample,dat1)
#  print(i)
}
estvssamp2(list(chain[,1],chain[,2],chain[,3],chain[,4],chain[,5],
                chain[,6],chain[,7],chain[,8],chain[,9],chain[,10]),7.2,7.35)

SEplotMulti(list(chain[,1],chain[,2],chain[,3],chain[,4],chain[,5],
                 chain[,6],chain[,7],chain[,8],chain[,9],chain[,10]))

plot(density(chain[,1]),main="Density Estimates
     with Different Starting Values", xlab="b1")
lines(density(chain[,2]),col="red")
lines(density(chain[,3]),col="blue")
lines(density(chain[,4]),col="orange")
lines(density(chain[,5]),col="purple")
lines(density(chain[,6]),col="seagreen")
lines(density(chain[,7]),col="green")
lines(density(chain[,8]),col="cyan")
lines(density(chain[,9]),col="violet")
lines(density(chain[,10]),col="pink")


bmmat(chain)
# est          se
# V1  7.282284 0.007286503
# V2  7.266963 0.006447643
# V3  7.263480 0.006901548
# V4  7.280927 0.007867693
# V5  7.263395 0.006449962
# V6  7.275987 0.007132946
# V7  7.283257 0.006971525
# V8  7.272098 0.006051213
# V9  7.282961 0.006517861
# V10 7.275817 0.006963899
# which are consistent enough.
#Autocorrelation



#####################################################
#############     Problem 2
#####################################################

###########################################################################################
#Log Likelihood Function (Same as in problem 1 since I didn't try to reduce the given parameters)
###########################################################################################
Loglikelihood<-function(b0,b1,lambda,sigma,data){
  mu<-b0+b1*data$V1
  single<-dexpgauss(data$V2,mu,sigma=sigma,lambda=lambda,log=TRUE)
  loglikelihood<-sum(single)
  return(loglikelihood)
}

###################################################################################################
#Log Prior for (b0,b1,lambda), using joint distribution since we assumed independence among parameters
#################################################################################################
Prior <- function(b0,b1,lambda){
  priorb0<-dnorm(b0,mean=0,sd=10,log=TRUE)
  priorb1<-dnorm(b1,mean=0,sd=10,log=TRUE)
  priorlambda <-dgamma(lambda,shape=0.01,scale=100,log=TRUE)
  return(priorb0+priorb1+priorlambda)
}
#########################################
#Log Posterior for (b0,b1,lambda)
#########################################
Posterior <- function(b0,b1,lambda,sigma,data){
  return (Loglikelihood(b0,b1,lambda,sigma,data) + Prior(b0,b1,lambda))
}
#########################################
#Proposal Functions (One Variable a time)
#########################################
proposalfunctionb0 <- function(b0current){rnorm(1,mean=b0current, sd= .5)}
proposalfunctionb1 <- function(b1current){rnorm(1,mean=b1current, sd= .5)}
proposalfunctionlambda <- function(lambdacurrent){rnorm(1,mean=lambdacurrent, sd= .5)}

###########################################
#Metropolis Update: One Variable at a time
###########################################
V_MH <- function(b0start,b1start,lambdastart,niter,data){
  chain = matrix(rep(NA,(niter+1)*3),ncol=3)
  chain[1,1] = b0start
  chain[1,2] = b1start
  chain[1,3] = lambdastart
#   acc1<-0
#   acc2<-0
#   acc3<-0
  for (i in 1:niter){
    #Update b0
    proposalb0 = proposalfunctionb0(chain[i,1])
    r<-exp(Posterior(proposalb0,chain[i,2],chain[i,3],1,data)-Posterior(chain[i,1],chain[i,2],chain[i,3],1,data))
    prob <- min(1,r)
    if (runif(1) < prob){
      chain[i+1,1] = proposalb0 #;acc1<-acc1+1
    }else{
      chain[i+1,1] = chain[i,1]
    }
    #Update b1
    proposalb1 = proposalfunctionb1(chain[i,2])
    r<-exp(Posterior(chain[i+1,1],proposalb1,chain[i,3],1,data)-Posterior(chain[i+1,1],chain[i,2],chain[i,3],1,data))
    prob <- min(1,r)
    if (runif(1) < prob){
      chain[i+1,2] = proposalb1 #;acc2<-acc2+1
    }else{
      chain[i+1,2] = chain[i,2]
    }
    #Update lambda
    proposallambda = proposalfunctionlambda(chain[i,3])
    if (proposallambda>0){
    r<-exp(Posterior(chain[i+1,1],chain[i+1,2],proposallambda,1,data)-Posterior(chain[i+1,1],chain[i+1,2],chain[i,3],1,data))
    prob <- min(1,r)
    if (runif(1) < prob){
      chain[i+1,3] = proposallambda #;acc3<-acc3+1
    }else{
      chain[i+1,3] = chain[i,3]
    }}else{chain[i+1,3] = chain[i,3]}
#Now we have one more MC update. Loop back for i+1.
  }
  return(
  #  list(
      chain
   #   ,c(acc1,acc2,acc3))
   )
}


#------Testing if the sampler works with simulated data. Incomplete. Please ignore------
# b0=-2
# b1=2
# x<-runif(1000,0,1)
# mu<-b0+b1*x
# y<-sapply(mu,rexpgauss,n=1,sigma=1,lambda=.5)
# data<-data.frame(cbind(x,y))
# colnames(data) <- c("V1", "V2")
# test0<-V_MH(0,-2,5,1000,data)
# bmmat(test0)
# test_02<-V_MH(-1.95,2.06,.58,100000,data)
# bmmat(test_02)
# ess(test_02)
# test1<-V_MH(0,-2,5,1000,data)
# bmmat(test1)
# test_12<-V_MH(-1.83,1.83,0.58,100000,data)
# bmmat(test_12)
# test2<-V_MH(0,-2,5,1000,data)
# bmmat(test2)
# test_22<-V_MH(-1.88,1.90,0.56,100000,data)
# bmmat(test_22)
# test2<-V_MH_2(10,-1,5,1000,data)
# bmmat(test2)
# apply(test_02,2,CI)
# estvssamp(test_02[,1])
# estvssamp(test_02[,2])
# estvssamp(test_22[,3])

chain_VATtrial<-V_MH(0,0,10,1000,dat2)
#Examine preliminary result to examine "good" starting values
bmmat(chain_VATtrial)
#      est         se
#V1 2.4138035 0.03373908
#V2 3.3275724 0.06898047
#V3 0.9753915 0.16088973
system.time(chain_VAT<-V_MH(2.4,3.3, 0.9,100000,dat2))
# user  system elapsed 
# 104.78    0.00  104.99 
apply(chain_VAT,2,mean)
# [1] 2.3468642 3.4608186 0.8049528
bmmat(chain_VAT)  
#    est          se
# V1 2.3530485 0.003030950
# V2 3.4507068 0.004194465
# V3 0.8050281 0.000982568
ess(chain_VAT)
# truncated after  45  lags
# [1] 6666.755

CI<-function(x){quantile(x,c(.025,.975))}
apply(chain_VAT,2,CI)
#          [,1]     [,2]      [,3]
# 2.5%  2.078179 3.042463 0.6967527
# 97.5% 2.608276 3.865280 0.9298125

par(mfrow=c(2,3))
estvssamp(chain_VAT[,1],plotname="mean estimate (b0)")
estvssamp(chain_VAT[,2],plotname="mean estimate (b1)")
estvssamp(chain_VAT[,3],plotname="mean estimate (lambda)")
SEplot(chain_VAT[,1],plotname="MCMCse(b0)")
SEplot(chain_VAT[,2],plotname="MCMCse(b1)")
SEplot(chain_VAT[,3],plotname="MCMCse(lambda)")
plot(1:100001,chain_VAT[,1],type="l",main="Trace of b0",ylab="b0")
plot(1:100001,chain_VAT[,2],type="l",main="Trace of b1",ylab="b1")
plot(1:100001,chain_VAT[,3],type="l",main="Trace of lambda",,ylab="lambda")
#Correlation Estimates
cor(chain_VAT[,1],chain_VAT[,2])
# [1] -0.7702103

par(mfrow=c(1,1))

CorrTracker(chain_VAT,1,2) 


acf(chain_VAT_dat3)


#Marginal Density Plots
par(mfrow=c(1,3))
plot(density(chain_VAT[,1]),col="blue",xlab="b0",main="")
lines(density(chain_VAT[,1][1:50000]),col="red")
plot(density(chain_VAT[,2]),col="blue",xlab="b1",main="")
lines(density(chain_VAT[,2][1:50000]),col="red")
plot(density(chain_VAT[,3]),col="blue",xlab="lambda",main="")
lines(density(chain_VAT[,3][1:50000]),col="red")


#5 different starting values 
start_a_bunch_b0<-seq(0,4,by=.8)[-1]
start_a_bunch_b1<-seq(1,5,by=.8)[-1]
start_a_bunch_lambda<-seq(0,2,by=.4)[-1]
nsample<-100000
chain_VAT_diff_start<-matrix(rep(NA,(nsample+1)*3*length(start_a_bunch_b0)),ncol=3*length(start_a_bunch_b0))
system.time(for(i in 1:length(start_a_bunch_b0)){
  chain_VAT_diff_start[,(3*(i-1)+1):(3*(i-1)+3)]<-V_MH(start_a_bunch_b0[i],start_a_bunch_b1[i],start_a_bunch_b0[i],nsample,dat2)
  #  print(i)
}
)
# user  system elapsed 
# 549.93    0.31  590.38 
save("chain_VAT_diff_start", file="chain_VAT_diff_start.RData")
par(mfrow=c(2,3))
estvssamp2(list(chain_VAT_diff_start[,1],chain_VAT_diff_start[,4],chain_VAT_diff_start[,7],
                chain_VAT_diff_start[,10],chain_VAT_diff_start[,13]),2.25,2.45,plotname="mean estimates (b0)")
estvssamp2(list(chain_VAT_diff_start[,2],chain_VAT_diff_start[,5],chain_VAT_diff_start[,8],
                chain_VAT_diff_start[,11],chain_VAT_diff_start[,14]),3.35,3.55,plotname="mean estimates (b1)")
estvssamp2(list(chain_VAT_diff_start[,3],chain_VAT_diff_start[,6],chain_VAT_diff_start[,9],
                chain_VAT_diff_start[,12],chain_VAT_diff_start[,15]),0.75,0.85,plotname="mean estimates (lambda)")


SEplotMulti(list(chain_VAT_diff_start[,1],chain_VAT_diff_start[,4],chain_VAT_diff_start[,7],
                chain_VAT_diff_start[,10],chain_VAT_diff_start[,13]),plotname="MCSE (Multiple chains) of b0")
SEplotMulti(list(chain_VAT_diff_start[,2],chain_VAT_diff_start[,5],chain_VAT_diff_start[,8],
                chain_VAT_diff_start[,11],chain_VAT_diff_start[,14]),plotname="MCSE (Multiple chains) of b1")
SEplotMulti(list(chain_VAT_diff_start[,3],chain_VAT_diff_start[,6],chain_VAT_diff_start[,9],
                chain_VAT_diff_start[,12],chain_VAT_diff_start[,15]),plotname="MCSE (Multiple chains) of lambda")

apply(chain_VAT_diff_start,2,CI)
bmmat(chain_VAT_diff_start)
par(mfrow=c(1,3))
# Density Estimates with Different Starting Values
plot(density(chain_VAT_diff_start[,1]), xlab="b0",main="")
lines(density(chain_VAT_diff_start[,4]),col="red")
lines(density(chain_VAT_diff_start[,7]),col="blue")
lines(density(chain_VAT_diff_start[,10]),col="orange")
lines(density(chain_VAT_diff_start[,13]),col="purple")
plot(density(chain_VAT_diff_start[,2]), xlab="b1",main="")
lines(density(chain_VAT_diff_start[,5]),col="red")
lines(density(chain_VAT_diff_start[,8]),col="blue")
lines(density(chain_VAT_diff_start[,11]),col="orange")
lines(density(chain_VAT_diff_start[,14]),col="purple")
plot(density(chain_VAT_diff_start[,3]), xlab="lambda",main="")
lines(density(chain_VAT_diff_start[,5]),col="red")
lines(density(chain_VAT_diff_start[,9]),col="blue")
lines(density(chain_VAT_diff_start[,12]),col="orange")
lines(density(chain_VAT_diff_start[,15]),col="purple")



###################################################################
########################     Problem 3
###################################################################
chain_VATtrial_dat3<-V_MH(0,0,10,1000,dat3)
#Examine preliminary result to determine "good" starting values
bmmat(chain_VATtrial_dat3)
#         est         se
# V1 0.2997937 0.11162696
# V2 2.5270271 0.06450963
# V3 0.4618372 0.23698271
system.time(chain_VAT_dat3<-V_MH(0.30,2.53, 0.46,80000,dat3))
#  user  system elapsed 
# 142.15    0.03  149.70 

apply(chain_VAT_dat3,2,mean)
bmmat(chain_VAT_dat3)  
#     est           se
# V1 0.1521851 0.0036291255
# V2 2.4689778 0.0053848736
# V3 0.1612765 0.0002211704

ess(chain_VAT_dat3)
# truncated after  44  lags
# [1] 6458.658

apply(chain_VAT_dat3,2,CI)
#           [,1]     [,2]      [,3]
# 2.5%  -0.1640115 1.902797 0.1505753
# 97.5%  0.4834631 3.007144 0.1724100

par(mfrow=c(1,3))
plot(density(chain_VAT_dat3[,1]),col="blue",xlab="b0",main="")
lines(density(chain_VAT_dat3[,1][1:40000]),col="red")
plot(density(chain_VAT_dat3[,2]),col="blue",xlab="b1",main="")
lines(density(chain_VAT_dat3[,2][1:40000]),col="red")
plot(density(chain_VAT_dat3[,3]),col="blue",xlab="lambda",main="")
lines(density(chain_VAT_dat3[,3][1:40000]),col="red")


################Problem 3 (c)###########################

#Proposal Functions (One Variable a time)
proposalfunctionb0 <- function(b0current){rnorm(1,mean=b0current, sd= .4)}
proposalfunctionb1 <- function(b1current){rnorm(1,mean=b1current, sd= .4)}
proposalfunctionlambda <- function(lambdacurrent){rnorm(1,mean=lambdacurrent, sd= .2)}
chain_VAT_dat3_1_trial<-V_MH(0,0,5,2500,dat3)
bmmat(chain_VAT_dat3_1_trial) 
system.time(chain_VAT_dat3_1<-V_MH(0.24,2.47, 0.22,80000,dat3))
bmmat(chain_VAT_dat3_1) 
#      est          se
# V1 0.1496128 0.003504169
# V2 2.4720692 0.005595749
# V3 0.1611404 0.000136184
apply(chain_VAT_dat3_1,2,CI)
#         [,1]     [,2]      [,3]
# 2.5%  -0.1826723 1.919325 0.1506744
# 97.5%  0.4655585 3.024107 0.1719166
ess(chain_VAT_dat3_1)
# truncated after  44  lags
# [1] 7171.046

par(mfrow=c(1,3))
plot(density(chain_VAT_dat3_1[,1]),col="blue",xlab="b0",main="")
lines(density(chain_VAT_dat3[,1][1:40000]),col="red")
plot(density(chain_VAT_dat3_1[,2]),col="blue",xlab="b1",main="")
lines(density(chain_VAT_dat3[,2][1:40000]),col="red")
plot(density(chain_VAT_dat3_1[,3]),col="blue",xlab="lambda",main="")
lines(density(chain_VAT_dat3[,3][1:40000]),col="red")
par(mfrow=c(2,2))
acf(chain_VAT_dat3[,1])
acf(chain_VAT_dat3_1[,1])
acf(chain_VAT_dat3[,2])
acf(chain_VAT_dat3_1[,2])
acf(chain_VAT_dat3[,3],main="lambda, proposal: N(0, sd=0.5)")
acf(chain_VAT_dat3_1[,3],main="lambda, proposal: N(0, sd=0.2)")
par(mfrow=c(1,2))
plot(1:80001,chain_VAT_dat3[,1],type="l",main="Proposal: N(0, sd=0.5)",ylab="b0")
plot(1:80001,chain_VAT_dat3_1[,1],type="l",main="Proposal: N(0, sd=0.4)",ylab="b0")
plot(1:80001,chain_VAT_dat3[,2],type="l",main="Proposal: N(0, sd=0.5)",ylab="b1")
plot(1:80001,chain_VAT_dat3_1[,2],type="l",main="Proposal: N(0, sd=0.2)",ylab="b1")
plot(1:80001,chain_VAT_dat3[,3],type="l",main="Proposal: N(0, sd=0.5)",ylab="lambda",ylim=c(.14,.22),)
plot(1:80001,chain_VAT_dat3_1[,3],type="l",main="Proposal: N(0, sd=0.2)",ylab="lambda")


###################################################################################################################
#################I tried using blocks but the algorithm did not improve too much, so it was discarded #############
################################################################################################################### 

# cov(chain_VAT[,1],chain_VAT[,2])
# var(chain_VAT[,1]);var(chain_VAT[,2])
# vcovmatrix<-matrix(c(0.018,-0.0214,-0.0214,0.429),ncol=2)
#
# V_MH_2 <- function(b0start,b1start,lambdastart,niter,data){
#   chain = matrix(rep(NA,(niter+1)*3),ncol=3)
#   chain[1,1] = b0start
#   chain[1,2] = b1start
#   chain[1,3] = lambdastart
#   for (i in 1:niter){
#     proposalb <- mvrnorm(1,c(chain[i,1],chain[i,2]),vcovmatrix)
#     r<-exp(Posterior(proposalb[1],proposalb[2],chain[i,3],1,data)-Posterior(chain[i,1],chain[i,2],chain[i,3],1,data))
#     prob <- min(1,r)
#     if (runif(1) < prob){
#       chain[i+1,1:2] = proposalb
#     }else{
#       chain[i+1,1:2] = chain[i,1:2]
#     }    
#     proposallambda = proposalfunctionlambda(chain[i,3])
#     if (proposallambda<=0){chain[i+1,3] = chain[i,3]}else{
#       r<-exp(Posterior(chain[i+1,1],chain[i+1,2],proposallambda,1,data)-Posterior(chain[i+1,1],chain[i+1,2],chain[i,3],1,data))
#       prob <- min(1,r)
#       if (runif(1) < prob){
#         chain[i+1,3] = proposallambda
#       }else{
#         chain[i+1,3] = chain[i,3]
#       }}
#     #   print(c(i,chain[i,]))
#   }
#   return(chain)
# }

# system.time(chain_VAT2_dat3<-V_MH_2(2.5,3.3, 0.98,100000,dat3))
# ess(chain_VAT2_dat3)




