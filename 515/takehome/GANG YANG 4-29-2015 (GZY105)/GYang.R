#Notice that the file contains the code for Q1,Q2,Q3 in order.
#Part of the code is bracated by if(FALSE){}, which is usually the test to answer part(e) in Q1/Q2
#In order to check that part, just change the FALSE to TRUE


source("http://www.stat.psu.edu/~mharan/batchmeans.R")   #load function to calculate MCMC SE
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")   #load functions related to EMG variables


## Markov chain Monte Carlo algorithm for posterior distribution of linear regression model
# load the data
data1 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
data2 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
data3 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")

##Q1 starts here
Samplesize = 10000        #Default MCMC Sample size, I may change it between questions

## Note: this function is not written in the most efficient way since its purpose is primarily instructive 

mhsampler = function(NUMIT=1000,dat=data,beta1_init = 7)#beta1_init is the initial value of beta1 of the markov chain
  {
    beta0 = 5     #fixed parameter beta0 in EMG
    lamda = 0.4   #fixed parameter lamda in EMG
    sigmaI = 1    #fixed parameter sigmaI in EMG
    mu = 0        #prior distribution expectation parameter, beta0 ~N(mu,sigma)
    sigma = 10
    tao = 1       #propose function for beta0 is normal distribution, tao is standard error

    ## set up
    ## NUMIT x 1 matrix to store Markov chain values
    ## each row corresponds to one of 1 parameters in order: beta1
    ## each column corresponds to a single state of the Markov chain
    mchain = matrix(NA, 1, NUMIT)
    acc = 0 # count number of accepted proposals (for beta1 only)
    
    ## starting values for Markov chain
    ## This is somewhat arbitrary but any method that produces reasonable values for each parameter is usually adequate.
    ## For instance, we can use approximate prior means or approximate MLEs.
    
     
    mchain[,1] = c(beta1_init)
    
    for (i in 2:NUMIT)
      {
        ## most upto date state for each parameter
        currbeta1 = mchain[1,i-1]
                
        ## sample from full conditional distribution of beta1 (Metropolis-Hastings update)
        propbeta1 = rnorm(1,currbeta1,tao) # draw one sample from normal distribution N(current position, tao)

        ## Metropolis accept-reject step (in log scale)
        logMHratio = sum(dexpgauss(dat[,2],propbeta1*dat[,1]+beta0,sigmaI,lamda,TRUE))+log(dnorm(propbeta1,mu,sigma))-sum(dexpgauss(dat[,2],currbeta1*dat[,1]+beta0,sigmaI,lamda,TRUE))-log(dnorm(currbeta1,mu,sigma))

        logalpha = min(0,logMHratio) # alpha = min(1,MHratio)
        if (log(runif(1))<logalpha) # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
          {
            acc = acc + 1 # increment count of accepted proposals
            currbeta1 = propbeta1
          }
              
        
        ## update chain with new values
        mchain[,i] = c(currbeta1)
        
      }

    cat("Markov chain algorithm ran for ",NUMIT,"iterations (acc.rate for beta1=",acc/(NUMIT-1),")\n")
    cat("Parameters are in order: beta1\n")
    return(mchain)
}

##code for Q1(b)-(d)
##change the TRUE to FALSE if you do not want to test this block any more
if (FALSE){
Samplesize =30000
MChain=mhsampler(Samplesize,data1,7)       #construct MC from initial value beta1=7
bm(MChain[1,])                             #calculate expectation and MCMC SE
quantile(MChain[1,],c(0.025,0.975))        #calculate confidential interval
plot(seq(1:Samplesize), MChain, xlim=c(0,Samplesize), type="b", cex=1, main="MC beta1 draw vs timestep N", xlab="N") 
plot(density(MChain[1,]))                  #density plot of MC draws of beta1
acf(MChain[1,])                            #calculate auto-correlation of beta1 draws
ess(MChain[1,])
}

##code ofr Q1(e)(2)
if (FALSE){
Sampleseq=seq(1000,10000,by=500)           #construct a sequence from 1000 to 10000 incremendted by 500
MCSE <- numeric(1)                         #initialize a vector to store MCMCSE for different sample size N
MCSE = 0
for (i in 1:length(Sampleseq)){            #for every sample size N
	MChain=mhsampler(Sampleseq[i],data1,7)   #construct a MC
      MCSE[i]<-bm(MChain)['se']                #calculate and store MCMC SE
}
plot(Sampleseq, MCSE, xlim=c(0,Samplesize), type="b", cex=1, main="MCSE vs sample size N", xlab="N") 
}

##code for Q1(e)(3)
if (FALSE){
Samplesize = 10000
#construct three chains from different initial condition for beta1 as 5,7,10 
MChain1=mhsampler(Samplesize,data1,5)
MChain2=mhsampler(Samplesize,data1,7)
MChain3=mhsampler(Samplesize,data1,10)
Ebeta11=matrix(NA, 1, Samplesize)          #initialize vector to store expectation averaged by draw up to N
Ebeta12=matrix(NA, 1, Samplesize)
Ebeta13=matrix(NA, 1, Samplesize)
for (j in 1:Samplesize){
   	Ebeta11[1,j]=sum(MChain1[1,1:j])/j   #calcualte expectation of beta1 up to different timestep j from 1 to N
      Ebeta12[1,j]=sum(MChain2[1,1:j])/j
      Ebeta13[1,j]=sum(MChain3[1,1:j])/j
}
matplot(seq(1:Samplesize), cbind(Ebeta11[1,],Ebeta12[1,],Ebeta13[1,]),xlim=c(0,Samplesize),col=1:3, type="b",pch=19, cex=1, main="MC estimated expectation of beta1 vs timestep N", xlab="N") 
legend("topright",c("beta1_init=5","beta1_init=7","beta1_init=10"),col=1:3,pch=19)
}

##Q2 start from here. 
##To code faster, I just define another function
mhsampler2 = function(NUMIT=1000,dat=data,beta0_init=5,beta1_init=7,lamda_init=0.4)
#parameters are samplesize, data of the model, initial value for beta0, beta1 and lamda
  {
    sigmaI = 1         #fixed parameter in EMG
    mu_beta0 = 0       #prior distribution parameter beta0~N(mu_beta0,sigma_beta0)
    sigma_beta0 = 10
    mu_beta1 = 0       #prior distribution parameter beta1~N(mu_beta1,sigma_beta1)
    sigma_beta1 = 10
    alpha_lamda = 0.01 #prior distribution parameter lamda~gamma(alpha_lamda,beta_lamda)
    beta_lamda = 100
    tao_beta0 = 0.3       #propose function is normal distribution, tao_beta0 is standard error
    tao_beta1 = 0.5       #propose function is normal distribution, tao_beta1 is standard error
    tao_lamda = 0.11      #propose function is normal distribution, tao_lamda is standard error

    ## set up
    ## NUMIT x 1 matrix to store Markov chain values
    ## each row corresponds to one of 1 parameters in order: beta1
    ## each column corresponds to a single state of the Markov chain
    mchain = matrix(NA, 3, NUMIT)
    acc_beta0 = 0 # count number of accepted proposals (for beta0,beta1 and lamda)
    acc_beta1 = 0
    acc_lamda = 0

    ## starting values for Markov chain
    ## This is somewhat arbitrary but any method that produces reasonable values for each parameter is usually adequate.
    ## For instance, we can use approximate prior means or approximate MLEs.
    mchain[,1] = c(beta0_init,beta1_init,lamda_init)
    
    for (i in 2:NUMIT)
      {
        ## most upto date state for each parameter
        currbeta0 = mchain[1,i-1]
        currbeta1 = mchain[2,i-1]
        currlamda = mchain[3,i-1]        
        
        ## sample from full conditional distribution of beta0 (Metropolis-Hastings update)
        propbeta0 = rnorm(1,currbeta0,tao_beta0) # draw one sample from normal distribution N(currentposition, tao_beta0)

        ## Metropolis accept-reject step (in log scale)
        logMHratio_beta0 = sum(dexpgauss(dat[,2],currbeta1*dat[,1]+propbeta0,sigmaI,currlamda,TRUE))+log(dnorm(propbeta0,mu_beta0,sigma_beta0))-sum(dexpgauss(dat[,2],currbeta1*dat[,1]+currbeta0,sigmaI,currlamda,TRUE))-log(dnorm(currbeta0,mu_beta0,sigma_beta0))

        logalpha_beta0 = min(0,logMHratio_beta0) # alpha = min(1,MHratio)
        if (log(runif(1))<logalpha_beta0) # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
          {
            acc_beta0 = acc_beta0 + 1 # increment count of accepted proposals
            currbeta0 = propbeta0
          }

        ## sample from full conditional distribution of beta1 (Metropolis-Hastings update)
        propbeta1 = rnorm(1,currbeta1,tao_beta1) # draw one sample from normal distribution N(currentposition, tao_beta1)


        ## Metropolis accept-reject step (in log scale)
        logMHratio_beta1 = sum(dexpgauss(dat[,2],propbeta1*dat[,1]+currbeta0,sigmaI,currlamda,TRUE))+log(dnorm(propbeta1,mu_beta1,sigma_beta1))-sum(dexpgauss(dat[,2],currbeta1*dat[,1]+currbeta0,sigmaI,currlamda,TRUE))-log(dnorm(currbeta1,mu_beta1,sigma_beta1))

        logalpha_beta1 = min(0,logMHratio_beta1) # alpha = min(1,MHratio)
        if (log(runif(1))<logalpha_beta1) # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
          {
            acc_beta1 = acc_beta1 + 1 # increment count of accepted proposals
            currbeta1 = propbeta1
          }
              
        ## sample from full conditional distribution of beta1 (Metropolis-Hastings update)
        proplamda = rnorm(1,currlamda,tao_lamda) # draw one sample from normal distribution N(currentposition, tao_lamda)
        
        
        ## Metropolis accept-reject step (in log scale)
        if (proplamda>0){  #notice lamda is not allowed to be smaller than zero
        	logMHratio_lamda = sum(dexpgauss(dat[,2],currbeta1*dat[,1]+currbeta0,sigmaI,proplamda,TRUE))+log(dgamma(proplamda,shape=alpha_lamda,scale=1/beta_lamda))-sum(dexpgauss(dat[,2],currbeta1*dat[,1]+currbeta0,sigmaI,currlamda,TRUE))-log(dgamma(currlamda,shape=alpha_lamda,scale=1/beta_lamda))
        } else {           #if lamda is smaller than zero, ratio should be 0, log0=-Inf
        	logMHratio_lamda = -Inf
        }

        logalpha_lamda = min(0,logMHratio_lamda) # alpha = min(1,MHratio)
        if (log(runif(1))<logalpha_lamda) # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
          {
            acc_lamda = acc_lamda + 1 # increment count of accepted proposals
            currlamda = proplamda
          }

        ## update chain with new values
        mchain[,i] = c(currbeta0,currbeta1,currlamda)
        
      }

    cat("Markov chain algorithm ran for ",NUMIT,"iterations (acc.rate for beta0,beta1,lamda=",acc_beta0/(NUMIT-1),",",acc_beta1/(NUMIT-1),",",acc_lamda/(NUMIT-1),")\n")
    cat("Parameters are in order: beta0,beta1,lamda\n")
    return(mchain)
}

##code for Q2 (b)-(d)
##change the FALSE below to TRUE to enable it!!!!!!!
if (FALSE){
Samplesize=10000*20
Q2MChain=mhsampler2(Samplesize,data2,2,3.5,0.6)     #initial condition is chosed based on several trials
bm(Q2MChain[1,])                                    #calcualte expectation and MCMC SE for beta0
bm(Q2MChain[2,])                                    #**** for beta1
bm(Q2MChain[3,])                                    #**** for lamda
quantile(Q2MChain[1,],c(0.025,0.975))               #calculate 95% confidential interval for beta0
quantile(Q2MChain[2,],c(0.025,0.975))               #**** for beta1
quantile(Q2MChain[3,],c(0.025,0.975))               #**** for lamda
cor(Q2MChain[1,],Q2MChain[2,])                      #correlation between beta0 and beta1
plot(density(Q2MChain[1,]))                         #marginal density plot of beta0
plot(density(Q2MChain[2,]))                         #marginal density plot of beta1
plot(density(Q2MChain[3,]))                         #marginal density plot of lamda
matplot(seq(1:Samplesize), cbind(Q2MChain[1,],Q2MChain[2,],Q2MChain[3,]),xlim=c(0,Samplesize),col=1:3, type="b",pch=19, cex=1, main="MC sampling vs timestep N", xlab="N") 
legend("topright",c("beta0","beta1","lamda"),col=1:3,pch=19)
acf(Q2MChain[1,],lag=60)                            #auto correlation for beta0
acf(Q2MChain[2,],lag=60)                            #auto correlation for beta1
acf(Q2MChain[3,])                                   #auto correlation for lamda
ess(Q2MChain[1,])
ess(Q2MChain[2,])
ess(Q2MChain[3,])
}

##code for Q2 (e)
if (FALSE){
Sampleseq=seq(2000,30000,by=2000)     #construct a sequence from 2000 to 30000 with increment 2000
Q2MCSE_beta0 <- numeric(1)            #initialize vector to store MCMC SE of beta0, beta1 and lamda
Q2MCSE_beta0 <- 0
Q2MCSE_beta1 <- numeric(1)
Q2MCSE_beta1 <- 0
Q2MCSE_lamda <- numeric(1)
Q2MCSE_lamda <- 0
for (k in 1:length(Sampleseq)){       #for different sample size
	Q2MChain=mhsampler2(Sampleseq[k],data2,2,3.5,0.6)     #construct a markov chain
      Q2MCSE_beta0[k]<-bm(Q2MChain[1,])['se']               #calculate MCMC SE for beta0, beta1 and lamda
      Q2MCSE_beta1[k]<-bm(Q2MChain[2,])['se']
      Q2MCSE_lamda[k]<-bm(Q2MChain[3,])['se'] 
}
matplot(Sampleseq, cbind(Q2MCSE_beta0,Q2MCSE_beta1,Q2MCSE_lamda),xlim=c(0,Sampleseq[k]),col=1:3, type="b",pch=19, cex=1, main="MCSE of beta0, beta1, lamda vs timestep N", xlab="N") 
legend("topright",c("beta0","beta1","lamda"),col=1:3,pch=19)
}

##code for Q2 (e)
if (FALSE){
Samplesize = 10000
#construct three different chains starting from different initial conditions
Q2MChain1=mhsampler2(Samplesize,data2,1,1,0.4)
Q2MChain2=mhsampler2(Samplesize,data2,2,3.5,0.6)
Q2MChain3=mhsampler2(Samplesize,data2,5,5,1)
Evec1=matrix(NA, 3, Samplesize)
Evec2=matrix(NA, 3, Samplesize)
Evec3=matrix(NA, 3, Samplesize)
for (j in 1:Samplesize){                 #calculate estimated expectation for beta0,beta1,lamda versus timestep j
   for (i in 1:3){
	Evec1[i,j]=sum(Q2MChain1[i,1:j])/j
      Evec2[i,j]=sum(Q2MChain2[i,1:j])/j
      Evec3[i,j]=sum(Q2MChain3[i,1:j])/j
   }
}
matplot(seq(1:Samplesize), cbind(Evec1[1,],Evec2[1,],Evec3[1,],Evec1[2,],Evec2[2,],Evec3[2,],Evec1[3,],Evec2[3,],Evec3[3,]),xlim=c(0,Samplesize),col=1:9 , type="b",pch=10, cex=1, main="MC estimated expectation of beta0,beta1,lamda vs timestep N", xlab="N") 
legend("topright",c("beta0_init1","beta0_init2","beta0_init3","beta1_init1","beta1_init2","beta1_init3","lamda_init1","lamda_init2","lamda_init3"),col=1:9, pch=10)
}


##code for Q3 starts from here
##To make the coding faster, I just create another function
##It is exactly the same function, expect that tao_beta0,tao_beta1,tao_lamda are different
##Thus annotation is partially neglected.

mhsampler3 = function(NUMIT=1000,dat=data,beta0_init=5,beta1_init=7,lamda_init=0.4)#beta1_init initial value of the markov chain
  {
    sigmaI = 1
    mu_beta0 = 0            #prior distribution parameter beta0~N(mu_beta0,sigma_beta0)
    sigma_beta0 = 10
    mu_beta1 = 0            #prior distribution parameter beta1~N(mu_beta1,sigma_beta1)
    sigma_beta1 = 10
    alpha_lamda = 0.01      #prior distribution parameter lamda~gamma(alpha_lamda,beta_lamda)
    beta_lamda = 100
    tao_beta0 = 0.5         #propose function is normal distribution, tao_beta0 is standard error
    tao_beta1 = 0.75        #propose function is normal distribution, tao_beta1 is standard error
    tao_lamda = 0.025       #propose function is normal distribution, tao_lamda is standard error
    
    ## set up
    ## NUMIT x 1 matrix to store Markov chain values
    ## each row corresponds to one of 1 parameters in order: beta1
    ## each column corresponds to a single state of the Markov chain
    mchain = matrix(NA, 3, NUMIT)
    acc_beta0 = 0 # count number of accepted proposals (for beta0,beta1 and lamda)
    acc_beta1 = 0
    acc_lamda = 0

    ## starting values for Markov chain
    ## This is somewhat arbitrary but any method that produces reasonable values for each parameter is usually adequate.
    ## For instance, we can use approximate prior means or approximate MLEs.
    
     
    mchain[,1] = c(beta0_init,beta1_init,lamda_init)
    
    for (i in 2:NUMIT)
      {
        ## most upto date state for each parameter
        currbeta0 = mchain[1,i-1]
        currbeta1 = mchain[2,i-1]
        currlamda = mchain[3,i-1]        
        
        ## sample from full conditional distribution of beta0 (Metropolis-Hastings update)
        propbeta0 = rnorm(1,currbeta0,tao_beta0) # draw one sample from normal distribution N(currentposition, tao_beta0)

        ## Metropolis accept-reject step (in log scale)
        logMHratio_beta0 = sum(dexpgauss(dat[,2],currbeta1*dat[,1]+propbeta0,sigmaI,currlamda,TRUE))+log(dnorm(propbeta0,mu_beta0,sigma_beta0))-sum(dexpgauss(dat[,2],currbeta1*dat[,1]+currbeta0,sigmaI,currlamda,TRUE))-log(dnorm(currbeta0,mu_beta0,sigma_beta0))

        logalpha_beta0 = min(0,logMHratio_beta0) # alpha = min(1,MHratio)
        if (log(runif(1))<logalpha_beta0) # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
          {
            acc_beta0 = acc_beta0 + 1 # increment count of accepted proposals
            currbeta0 = propbeta0
          }

        ## sample from full conditional distribution of beta1 (Metropolis-Hastings update)
        propbeta1 = rnorm(1,currbeta1,tao_beta1) # draw one sample from normal distribution N(currentposition, tao_beta1)

        ## Metropolis accept-reject step (in log scale)
        logMHratio_beta1 = sum(dexpgauss(dat[,2],propbeta1*dat[,1]+currbeta0,sigmaI,currlamda,TRUE))+log(dnorm(propbeta1,mu_beta1,sigma_beta1))-sum(dexpgauss(dat[,2],currbeta1*dat[,1]+currbeta0,sigmaI,currlamda,TRUE))-log(dnorm(currbeta1,mu_beta1,sigma_beta1))

        logalpha_beta1 = min(0,logMHratio_beta1) # alpha = min(1,MHratio)
        if (log(runif(1))<logalpha_beta1) # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
          {
            acc_beta1 = acc_beta1 + 1 # increment count of accepted proposals
            currbeta1 = propbeta1
          }
              
        ## sample from full conditional distribution of beta1 (Metropolis-Hastings update)
        proplamda = rnorm(1,currlamda,tao_lamda) # draw one sample from normal distribution N(currentposition, tao_lamda)
        
        ## Metropolis accept-reject step (in log scale)
        if (proplamda>0){
        	logMHratio_lamda = sum(dexpgauss(dat[,2],currbeta1*dat[,1]+currbeta0,sigmaI,proplamda,TRUE))+log(dgamma(proplamda,shape=alpha_lamda,scale=1/beta_lamda))-sum(dexpgauss(dat[,2],currbeta1*dat[,1]+currbeta0,sigmaI,currlamda,TRUE))-log(dgamma(currlamda,shape=alpha_lamda,scale=1/beta_lamda))
        } else {
        	logMHratio_lamda = -Inf
        }

        logalpha_lamda = min(0,logMHratio_lamda) # alpha = min(1,MHratio)
        if (log(runif(1))<logalpha_lamda) # accept if unif(0,1)<alpha, i.e. accept with probability alpha, else stay at current state
          {
            acc_lamda = acc_lamda + 1 # increment count of accepted proposals
            currlamda = proplamda
          }

        ## update chain with new values
        mchain[,i] = c(currbeta0,currbeta1,currlamda)
        
      }

    cat("Markov chain algorithm ran for ",NUMIT,"iterations (acc.rate for beta0,beta1,lamda=",acc_beta0/(NUMIT-1),",",acc_beta1/(NUMIT-1),",",acc_lamda/(NUMIT-1),")\n")
    cat("Parameters are in order: beta0,beta1,lamda\n")
    return(mchain)
}

##code for Q3 (a)-(b)
##change the FALSE below to TRUE to enable it!!!!!!!
if (FALSE){
Samplesize = 200000
Q3MChain=mhsampler3(Samplesize,data3,0.1,2.5,0.15)   #initial condition is chosed based on several trials
bm(Q3MChain[1,])                                     #calcualte expectation and MCMC SE for beta0,beta1 and lamda
bm(Q3MChain[2,])
bm(Q3MChain[3,])
quantile(Q3MChain[1,],c(0.025,0.975))                #calculate 95% confidential interval for beta0,beta1 and lamda
quantile(Q3MChain[2,],c(0.025,0.975))
quantile(Q3MChain[3,],c(0.025,0.975))
#cor(Q3MChain[1,],Q3MChain[2,])
plot(density(Q3MChain[1,]))                          ##marginal density plot of beta0
plot(density(Q3MChain[2,]))
plot(density(Q3MChain[3,]))
matplot(seq(1:Samplesize), cbind(Q3MChain[1,],Q3MChain[2,],Q3MChain[3,]),xlim=c(0,Samplesize),col=1:3, type="b",pch=19, cex=1, main="MC sampling vs timestep N", xlab="N") 
legend("topright",c("beta0","beta1","lamda"),col=1:3,pch=19)
#acf(Q3MChain[1,],lag=60)
#acf(Q3MChain[2,],lag=60)
#acf(Q3MChain[3,])
ess(Q3MChain[1,])
ess(Q3MChain[2,])
ess(Q3MChain[3,])
}

##code for Q3 to test the validity of the MCMC method
##this is to see how MCMC SE changes with sample size N
if (FALSE){
Sampleseq=seq(1000,20000,by=1000)
Q3MCSE_beta0 <- numeric(1)
Q3MCSE_beta0 <- 0
Q3MCSE_beta1 <- numeric(1)
Q3MCSE_beta1 <- 0
Q3MCSE_lamda <- numeric(1)
Q3MCSE_lamda <- 0
for (k in 1:length(Sampleseq)){
	Q3MChain=mhsampler3(Sampleseq[k],data2,0.1,2.5,0.15)
      Q3MCSE_beta0[k]<-bm(Q3MChain[1,])['se']
      Q3MCSE_beta1[k]<-bm(Q3MChain[2,])['se']
      Q3MCSE_lamda[k]<-bm(Q3MChain[3,])['se'] 
}
matplot(Sampleseq, cbind(Q3MCSE_beta0,Q3MCSE_beta1,Q3MCSE_lamda),xlim=c(0,Sampleseq[k]),col=1:3, type="b",pch=19, cex=1, main="MCSE of beta0, beta1, lamda vs timestep N", xlab="N") 
legend("topright",c("beta0","beta1","lamda"),col=1:3,pch=19)
}

##this is to see how expectation of paramters changes with timestep N
if (FALSE){
Samplesize = 10000
Q3MChain1=mhsampler3(Samplesize,data3,-1,1,0.05)
Q3MChain2=mhsampler3(Samplesize,data3,0.1,2.5,0.15)
Q3MChain3=mhsampler3(Samplesize,data3,4,4,1)
Evec1=matrix(NA, 3, Samplesize)
Evec2=matrix(NA, 3, Samplesize)
Evec3=matrix(NA, 3, Samplesize)
for (j in 1:Samplesize){
   for (i in 1:3){
	Evec1[i,j]=sum(Q3MChain1[i,1:j])/j
      Evec2[i,j]=sum(Q3MChain2[i,1:j])/j
      Evec3[i,j]=sum(Q3MChain3[i,1:j])/j
   }
}
matplot(seq(1:Samplesize), cbind(Evec1[1,],Evec2[1,],Evec3[1,],Evec1[2,],Evec2[2,],Evec3[2,],Evec1[3,],Evec2[3,],Evec3[3,]),xlim=c(0,Samplesize),col=1:9 , type="b",pch=10, cex=1, main="MC estimated expectation of beta0,beta1,lamda vs timestep N", xlab="N") 
legend("topright",c("beta0_init1","beta0_init2","beta0_init3","beta1_init1","beta1_init2","beta1_init3","lamda_init1","lamda_init2","lamda_init3"),col=1:9, pch=10)
}

