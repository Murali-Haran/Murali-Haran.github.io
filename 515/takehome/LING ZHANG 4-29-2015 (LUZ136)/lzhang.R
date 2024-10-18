# Read data #1, #2 and #3 
EMG1=read.table("EMG1.dat")
EMG2=read.table("EMG2.dat")
EMG3=read.table("EMG3.dat")
X1=c(EMG1[,1])
Y1=c(EMG1[,2])
X2=c(EMG2[,1])
Y2=c(EMG2[,2])
X3=c(EMG3[,1])
Y3=c(EMG3[,2])

######################################################################################################################
#Problem1 (b)
######################################################################################################################

#Generate MCMC sample from M-H algorithm

#Set initial value beta10=0 & Sample size N=5000
beta10=0
beta11=0
beta1=beta10
N=50000

for (i in 1:N)
{
  #Get joint distribution density of beta10
  u1=5+beta10*X1
  dY11=dexpgauss(Y1,u1,sigma = 1, lambda = 0.4, log = FALSE)
  f1=prod(dY11)*dnorm(beta10,0,10)
  
  #Generate beta11 from proposal N(beta10, 1)
  beta11=rnorm(1,beta10,1)
  
  #Get joint distribution density of beta11
  u2=5+beta11*X1
  dY12=dexpgauss(Y1,u2,sigma = 1, lambda = 0.4, log = FALSE)
  f2=prod(dY12)*dnorm(beta11,0,10)
  
  #Get acceptance probability
  alpha=min(1, f2/f1)
  
  #Reject or Accept the Proposal and get a new sample
  uu=runif(1,0,1)
  if (uu<alpha)
  {
    beta10=beta11
  }
  beta1=c(beta1,beta10)
    
}

#Get estimate of the posterior expectation of beta1 and its standard error
bm(beta1)


######################################################################################################################
#Problem1 (c)
######################################################################################################################

#Get a 95% credible interval for beta1 based on MCMC sample
quantile(beta1,c(0.025,0.975))

######################################################################################################################
#Problem1 (d)
######################################################################################################################
#     &
######################################################################################################################
#Problem1 (e)
######################################################################################################################

########################################################
#For starting value senario One::initial value beta1=0 #
########################################################

#Run the code for sample size N from 1000 to 50000 with increment 1000 and records every step
u1_hat=rep(0,50)
u1_se=rep(0,50)
MCsize=seq(1000,50000,by=1000)

for (i in 1:50)
{
  #Set initial beta1=0
  beta10=0
  beta11=0
  beta1=beta10
  N=1000*i
  
  for (j in 1:N)
  {
  #Get joint distribution density of beta10
  u1=5+beta10*X1
  dY11=dexpgauss(Y1,u1,sigma = 1, lambda = 0.4, log = FALSE)
  f1=prod(dY11)*dnorm(beta10,0,10)
  
  #Generate beta11 from proposal N(beta10, 1)
  beta11=rnorm(1,beta10,1)
  
  #Get joint distribution density of beta11
  u2=5+beta11*X1
  dY12=dexpgauss(Y1,u2,sigma = 1, lambda = 0.4, log = FALSE)
  f2=prod(dY12)*dnorm(beta11,0,10)
  
  #Get acceptance probability
  alpha=min(1, f2/f1)
  
  #Reject or Accept the Proposal and get a new sample
  uu=runif(1,0,1)
  if (uu<alpha)
  {
    beta10=beta11
  }
  beta1=c(beta1,beta10)
  
  }

#Get estimate of the posterior expectation of beta1 and its standard error when sample size is 100*i
  u1_hat[i]=bm(beta1)$est
  u1_se[i]=bm(beta1)$se
}

#Get MCMC sample for scenario one
beta1_0=beta1

########################################################
#For starting value senario Two::initial value beta1=4 #
########################################################

#Run the code for sample size N from 1000 to 50000 with increment 1000 and records every step
u1_hat1=rep(0,50)
u1_se1=rep(0,50)
MCsize=seq(1000,50000,by=1000)

for (i in 1:50)
{
  #Set initial beta1=4
  beta10=4
  beta11=4
  beta1=beta10
  N=1000*i
  
  for (j in 1:N)
  {
    #Get joint distribution density of beta10
    u1=5+beta10*X1
    dY11=dexpgauss(Y1,u1,sigma = 1, lambda = 0.4, log = FALSE)
    f1=prod(dY11)*dnorm(beta10,0,10)
    
    #Generate beta11 from proposal N(beta10, 1)
    beta11=rnorm(1,beta10,1)
    
    #Get joint distribution density of beta11
    u2=5+beta11*X1
    dY12=dexpgauss(Y1,u2,sigma = 1, lambda = 0.4, log = FALSE)
    f2=prod(dY12)*dnorm(beta11,0,10)
    
    #Get acceptance probability
    alpha=min(1, f2/f1)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta10=beta11
    }
    beta1=c(beta1,beta10)
    
  }
  
  #Get estimate of the posterior expectation of beta1 and its standard error
  u1_hat1[i]=bm(beta1)$est
  u1_se1[i]=bm(beta1)$se
}
#Get MCMC sample for scenario two
beta1_1=beta1

#########################################################
#For starting value senario Three:initial value beta1=8 #
#########################################################

#Run the code for sample size N from 1000 to 50000 with increment 1000 and records every step
u1_hat2=rep(0,50)
u1_se2=rep(0,50)
MCsize=seq(1000,50000,by=1000)

for (i in 1:50)
{
  #Set initial beta1=8
  beta10=8
  beta11=8
  beta1=beta10
  N=1000*i
  
  for (j in 1:N)
  {
    #Get joint distribution density of beta10
    u1=5+beta10*X1
    dY11=dexpgauss(Y1,u1,sigma = 1, lambda = 0.4, log = FALSE)
    f1=prod(dY11)*dnorm(beta10,0,10)
    
    #Generate beta11 from proposal N(beta10, 1)
    beta11=rnorm(1,beta10,1)
    
    #Get joint distribution density of beta11
    u2=5+beta11*X1
    dY12=dexpgauss(Y1,u2,sigma = 1, lambda = 0.4, log = FALSE)
    f2=prod(dY12)*dnorm(beta11,0,10)
    
    #Get acceptance probability
    alpha=min(1, f2/f1)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta10=beta11
    }
    beta1=c(beta1,beta10)
    
  }
  
  #Get estimate of the posterior expectation of beta1 and its standard error
  u1_hat2[i]=bm(beta1)$est
  u1_se2[i]=bm(beta1)$se
}
#Get MCMC sample for scenario three
beta1_2=beta1

#Plot Density Plots of Parameters and Supporting Plots for the Accuracy of Our Approximations
par(mfrow=c(1,4))
plot(density(beta1_0),main="Smoothed Density Plot of Beta1")
points(density(beta1_1),col="red",type="l")
points(density(beta1_2),col="green",type="l")
plot(MCsize,u1_hat,type="l",main="Estimate vs Sample Size")
points(MCsize,u1_hat1,col="red",type="l")
points(MCsize,u1_hat2,col="green",type="l")
plot(MCsize,u1_se,type="l",main="MCMC SE vs Sample Size")
points(MCsize,u1_se1,col="red",type="l")
points(MCsize,u1_se2,col="green",type="l")
acf(beta1,main="Acf of MCMC Sample Beta1 ")

#Give estimate effective sample sizes
ess(beta1_0)
ess(beta1_1)
ess(beta1_2)

######################################################################################################################
#Problem2 (b)
######################################################################################################################

#Set initial values
beta10=7
beta11=7
beta00=5
beta01=5
lambda0=0.4
lambda1=0.4
beta0=beta00
beta1=beta10
lambda=lambda0

#Set sample size N=200000
N=300000

for (i in 1:N)
{
  
  #Update beta1
  #Get distribution density of Y2 given beta10
  u1=beta00+beta10*X2
  dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
  f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
  
  #Generate beta11 from proposal N(beta10, 0.7)
  beta11=rnorm(1,beta10,0.7)
  
  #Get distribution density of Y2 given beta11
  u2=beta00+beta11*X2
  dY12=dexpgauss(Y2,u2,sigma = 1, lambda = lambda0, log = FALSE)
  f2=sum(log(dY12))+log(dnorm(beta11,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
  
  r=exp(f2-f1)
  #Get acceptance probability
  alpha=min(1,r)
  
  #Reject or Accept the Proposal and get a new sample
  uu=runif(1,0,1)
  if (uu<alpha)
  {
    beta10=beta11
  }
  beta1=c(beta1,beta10)
  
  #Update beta0
  #Get distribution density of Y2 given beta00
  u1=beta00+beta10*X2
  dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
  f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
  
  #Generate beta11 from proposal N(beta00, 0.7)
  beta01=rnorm(1,beta00,0.7)
  
  #Get distribution density of Y2 given beta11
  u2=beta01+beta10*X2
  dY12=dexpgauss(Y2,u2,sigma = 1, lambda = lambda0, log = FALSE)
  f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta01,0,10))+log(dgamma(lambda0,0.01,0.01))
  
  r=exp(f2-f1)
  #Get acceptance probability
  alpha=min(1, r)
  
  #Reject or Accept the Proposal and get a new sample
  uu=runif(1,0,1)
  if (uu<alpha)
  {
    beta00=beta01
  }
  
  beta0=c(beta0,beta00)
   
  #Update lambda
  #Get distribution density of Y2 given lambda0
  u1=beta00+beta10*X2
  dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
  f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
  
  #Generate lambda1 from proposal U(max(0, lambda0-0.3), lambda0+0.3)
  lambda1=runif(1,max(0,lambda0-0.3),lambda0+0.3)
  
  #Get distribution density of Y2 given lambda1
  dY12=dexpgauss(Y2,u1,sigma = 1, lambda = lambda1, log = FALSE)
  f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda1,0.01,0.01))
  
  r=exp(f2-f1)
  #Get acceptance probability
  alpha=min(1, r)
  
  #Reject or Accept the Proposal and get a new sample
  uu=runif(1,0,1)
  if (uu<alpha)
  {
    lambda0=lambda1
  }
  lambda=c(lambda,lambda0)
  
}

#Get estimate of the posterior expectation of beta1, beta0, lambda and their standard errors
bm(beta1)
bm(beta0)
bm(lambda)

#Get 95% credible intervals for beta1, beta0 and lambda
quantile(beta1,c(0.025,0.975))
quantile(beta0,c(0.025,0.975))
quantile(lambda,c(0.025,0.975))

######################################################################################################################
#Problem2 (c)
######################################################################################################################

#Correlation between beta1 and beta0
cor(beta1,beta0)

######################################################################################################################
#Problem2 (d)
######################################################################################################################
#   &
######################################################################################################################
#Problem2 (e)
######################################################################################################################

###############################################################################
#For starting value senario One:initial value beta1=7, beta0=5 and lambda=0.4 #
###############################################################################

#Run the code for sample size N from 6000 to 300000 with increment 6000 and records every step for this scenario
MCsize=seq(6000,300000,by=6000)

u1_hat=rep(0,50)
u1_se=rep(0,50)

u0_hat=rep(0,50)
u0_se=rep(0,50)

lambda_hat=rep(0,50)
lambda_se=rep(0,50)

#Set initial values
beta10=7
beta11=7
beta00=5
beta01=5
lambda0=0.4
lambda1=0.4
beta0=beta00
beta1=beta10
lambda=lambda0
N=6000

for (i in 1:50)
{
  for (j in 1:N)
  {
    
    #Update beta1
    #Get distribution density of Y2 given beta10
    u1=beta00+beta10*X2
    dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta10, 0.7)
    beta11=rnorm(1,beta10,0.7)
    
    #Get distribution density of Y2 given beta11
    u2=beta00+beta11*X2
    dY12=dexpgauss(Y2,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta11,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1,r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta10=beta11
    }
    beta1=c(beta1,beta10)
    
    
    #Update beta0
    #Get distribution density of Y2 given beta00
    u1=beta00+beta10*X2
    dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta00, 0.7)
    beta01=rnorm(1,beta00,0.7)
    
    #Get distribution density of Y2 given beta11
    u2=beta01+beta10*X2
    dY12=dexpgauss(Y2,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta01,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta00=beta01
    }
    beta0=c(beta0,beta00)
    
    #Update lambda
    #Get distribution density of Y2 given lambda0
    u1=beta00+beta10*X2
    dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate lambda1 from proposal U(max(0,lambda0-0.3),lambda0+0.3)
    lambda1=runif(1,max(0,lambda0-0.3),lambda0+0.3)
    
    #Get distribution density of Y2 given lambda1
    dY12=dexpgauss(Y2,u1,sigma = 1, lambda = lambda1, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda1,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      lambda0=lambda1
    }
    lambda=c(lambda,lambda0)
    
  }
  
  #Get estimate of the posterior expectation of beta1, beta0, lambda and their standard errors  
  u1_hat[i]=bm(beta1)$est
  u1_se[i]=bm(beta1)$se
  u0_hat[i]=bm(beta0)$est
  u0_se[i]=bm(beta0)$se 
  lambda_hat[i]=bm(lambda)$est
  lambda_se[i]=bm(lambda)$se
}

#Record MCMC sample for all the parameters of this scenario
beta0_0=beta0
beta1_0=beta1
lambda_0=lambda

###############################################################################
#For starting value senario Two:initial value beta1=4, beta0=3 and lambda=0.3 #
###############################################################################

#Run the code for sample size N from 6000 to 300000 with increment 6000 and records every step for this scenario
MCsize=seq(6000,300000,by=6000)

u1_hat1=rep(0,50)
u1_se1=rep(0,50)

u0_hat1=rep(0,50)
u0_se1=rep(0,50)

lambda_hat1=rep(0,50)
lambda_se1=rep(0,50)

#Set initial values
beta10=4
beta11=4
beta00=3
beta01=3
lambda0=0.3
lambda1=0.3
beta0=beta00
beta1=beta10
lambda=lambda0
N=6000

for (i in 1:50)
{
  for (j in 1:N)
  {
    
    #Update beta1
    #Get distribution density of Y2 given beta10
    u1=beta00+beta10*X2
    dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta10, 0.7)
    beta11=rnorm(1,beta10,0.7)
    
    #Get distribution density of Y2 given beta11
    u2=beta00+beta11*X2
    dY12=dexpgauss(Y2,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta11,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1,r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta10=beta11
    }
    beta1=c(beta1,beta10)
    
    
    #Update beta0
    #Get distribution density of Y2 given beta00
    u1=beta00+beta10*X2
    dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta00, 0.7)
    beta01=rnorm(1,beta00,0.7)
    
    #Get distribution density of Y2 given beta11
    u2=beta01+beta10*X2
    dY12=dexpgauss(Y2,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta01,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta00=beta01
    }
    beta0=c(beta0,beta00)
    
    #Update lambda
    #Get distribution density of Y2 given lambda0
    u1=beta00+beta10*X2
    dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate lambda1 from proposal U(max(0,lambda0-0.3),lambda0+0.3)
    lambda1=runif(1,max(0,lambda0-0.3),lambda0+0.3)
    
    #Get distribution density of Y2 given lambda1
    dY12=dexpgauss(Y2,u1,sigma = 1, lambda = lambda1, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda1,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      lambda0=lambda1
    }
    lambda=c(lambda,lambda0)
    
  }
  
  #Get estimate of the posterior expectation of beta1, beta0, lambda and their standard errors  
  u1_hat1[i]=bm(beta1)$est
  u1_se1[i]=bm(beta1)$se
  u0_hat1[i]=bm(beta0)$est
  u0_se1[i]=bm(beta0)$se 
  lambda_hat1[i]=bm(lambda)$est
  lambda_se1[i]=bm(lambda)$se
}

#Record MCMC sample for all the parameters of this scenario
beta0_1=beta0
beta1_1=beta1
lambda_1=lambda

################################################################################
#For starting value senario Three:initial value beta1=1,beta0=1 and lambda=0.2 #
################################################################################

#Run the code for sample size N from 6000 to 300000 with increment 6000 and records every step for this scenario
MCsize=seq(6000,300000,by=6000)

u1_hat2=rep(0,50)
u1_se2=rep(0,50)

u0_hat2=rep(0,50)
u0_se2=rep(0,50)

lambda_hat2=rep(0,50)
lambda_se2=rep(0,50)

#Set initial values
beta10=1
beta11=1
beta00=1
beta01=1
lambda0=0.2
lambda1=0.2
beta0=beta00
beta1=beta10
lambda=lambda0
N=6000

for (i in 1:50)
{
  for (j in 1:N)
  {
    
    #Update beta1
    #Get distribution density of Y2 given beta10
    u1=beta00+beta10*X2
    dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta10, 0.7)
    beta11=rnorm(1,beta10,0.7)
    
    #Get distribution density of Y2 given beta11
    u2=beta00+beta11*X2
    dY12=dexpgauss(Y2,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta11,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1,r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta10=beta11
    }
    beta1=c(beta1,beta10)
    
    
    #Update beta0
    #Get distribution density of Y2 given beta00
    u1=beta00+beta10*X2
    dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta00, 0.7)
    beta01=rnorm(1,beta00,0.7)
    
    #Get distribution density of Y2 given beta11
    u2=beta01+beta10*X2
    dY12=dexpgauss(Y2,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta01,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta00=beta01
    }
    beta0=c(beta0,beta00)
    
    #Update lambda
    #Get distribution density of Y2 given lambda0
    u1=beta00+beta10*X2
    dY11=dexpgauss(Y2,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate lambda1 from proposal U(max(0,lambda0-0.3),lambda0+0.3)
    lambda1=runif(1,max(0,lambda0-0.3),lambda0+0.3)
    
    #Get distribution density of Y2 given lambda1
    dY12=dexpgauss(Y2,u1,sigma = 1, lambda = lambda1, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda1,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      lambda0=lambda1
    }
    lambda=c(lambda,lambda0)
    
  }
  
  #Get estimate of the posterior expectation of beta1, beta0, lambda and their standard errors  
  u1_hat2[i]=bm(beta1)$est
  u1_se2[i]=bm(beta1)$se
  u0_hat2[i]=bm(beta0)$est
  u0_se2[i]=bm(beta0)$se 
  lambda_hat2[i]=bm(lambda)$est
  lambda_se2[i]=bm(lambda)$se
}

#Record MCMC sample for all the parameters of this scenario
beta0_2=beta0
beta1_2=beta1
lambda_2=lambda

#Plot support plots to show how accurate my approach is
par(mfrow=c(3,4))

#Support plots of beta0
plot(density(beta0_0),main="Density Plot of Beta0")
points(density(beta0_1),type="l",col="red")
points(density(beta0_2),type="l",col="green")
plot(MCsize,u0_hat,type="l",ylim=c(2.33,2.36),main="Estimate of Beta0 vs Sample Size")
points(MCsize,u0_hat1,type="l",col="red")
points(MCsize,u0_hat2,type="l",col="green")
plot(MCsize,u0_se,type="l",main="SE of Beta0 vs Sample Size")
points(MCsize,u0_se1,type="l",col="red")
points(MCsize,u0_se2,type="l",col="green")
acf(beta0_0,main="Acf of MCMC Sample Beta0 ")

#Give estimate effective sample sizes of beta0
ess(beta0_0)
ess(beta0_1)
ess(beta0_2)


#Support plots of beta1
plot(density(beta1_0),main="Density Plot of Beta1")
points(density(beta1_1),type="l",col="red")
points(density(beta1_2),type="l",col="green")
plot(MCsize,u1_hat,type="l",ylim=c(3.45,3.48),main="Estimate of Beta1 vs Sample Size")
points(MCsize,u1_hat1,type="l",col="red")
points(MCsize,u1_hat2,type="l",col="green")
plot(MCsize,u1_se,type="l",main="SE of Beta1 vs Sample Size")
points(MCsize,u1_se1,type="l",col="red")
points(MCsize,u1_se2,type="l",col="green")
acf(beta1_0,main="Acf of MCMC Sample Beta0 ")

#Give estimate effective sample sizes of beta1
ess(beta1_0)
ess(beta1_1)
ess(beta1_2)


#Support plots of lambda
plot(density(lambda_0),main="Density Plot of lambda")
points(density(lambda_1),type="l",col="red")
points(density(lambda_2),type="l",col="green")
plot(MCsize,lambda_hat,type="l",ylim=c(0.8,0.81),main="Estimate of Lambda vs Sample Size")
points(MCsize,lambda_hat1,type="l",col="red")
points(MCsize,lambda_hat2,type="l",col="green")
plot(MCsize,lambda_se,type="l",main="SE of Lambda vs Sample Size")
points(MCsize,lambda_se1,type="l",col="red")
points(MCsize,lambda_se2,type="l",col="green")
acf(lambda_0,main="Acf of MCMC Sample lambda ")

#Give estimate effective sample sizes of lambda
ess(lambda_0)
ess(lambda_1)
ess(lambda_2)



######################################################################################################################
#Problem3 (a)
######################################################################################################################

#Set initial values
beta10=7
beta11=7
beta00=5
beta01=5
lambda0=0.4
lambda1=0.4
beta0=beta00
beta1=beta10
lambda=lambda0

N=300000

for (i in 1:N)
{
  
  #Update beta1
  #Get distribution density of Y3 given beta10
  u1=beta00+beta10*X3
  dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
  f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
  
  #Generate beta11 from proposal N(beta10, 1)
  beta11=rnorm(1,beta10,1)
  
  #Get distribution density of Y3 given beta11
  u2=beta00+beta11*X3
  dY12=dexpgauss(Y3,u2,sigma = 1, lambda = lambda0, log = FALSE)
  f2=sum(log(dY12))+log(dnorm(beta11,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
  
  r=exp(f2-f1)
  #Get acceptance probability
  alpha=min(1,r)
  
  #Reject or Accept the Proposal and get a new sample
  uu=runif(1,0,1)
  if (uu<alpha)
  {
    beta10=beta11
  }
  beta1=c(beta1,beta10)
  
  
  #Update beta0
  #Get distribution density of Y3 given beta00
  u1=beta00+beta10*X3
  dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
  f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
  
  #Generate beta11 from proposal N(beta00, 1)
  beta01=rnorm(1,beta00,1)
  
  #Get distribution density of Y3 given beta11
  u2=beta01+beta10*X3
  dY12=dexpgauss(Y3,u2,sigma = 1, lambda = lambda0, log = FALSE)
  f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta01,0,10))+log(dgamma(lambda0,0.01,0.01))
  
  r=exp(f2-f1)
  #Get acceptance probability
  alpha=min(1, r)
  
  #Reject or Accept the Proposal and get a new sample
  uu=runif(1,0,1)
  if (uu<alpha)
  {
    beta00=beta01
  }
  beta0=c(beta0,beta00)
  
  
  #Update lambda
  #Get distribution density of Y3 given lambda0
  u1=beta00+beta10*X3
  dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
  f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
  
  #Generate lambda1 from proposal U(0, 2*lambda0)
  lambda1=runif(1,max(0,lambda0-0.2),lambda0+0.2)
  
  #Get distribution density of Y3 given lambda1
  dY12=dexpgauss(Y3,u1,sigma = 1, lambda = lambda1, log = FALSE)
  f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda1,0.01,0.01))
  
  r=exp(f2-f1)
  #Get acceptance probability
  alpha=min(1, r)
  
  #Reject or Accept the Proposal and get a new sample
  uu=runif(1,0,1)
  if (uu<alpha)
  {
    lambda0=lambda1
  }
  lambda=c(lambda,lambda0)
  
}

#Get estimate of the posterior expectation of beta1, beta0, lambda and their standard errors
bm(beta1)
bm(beta0)
bm(lambda)

#Get 95% credible intervals for beta1, beta0 and lambda
quantile(beta1,c(0.025,0.975))
quantile(beta0,c(0.025,0.975))
quantile(lambda,c(0.025,0.975))

######################################################################################################################
#Problem3 (b)
######################################################################################################################
#        &
######################################################################################################################
#Problem3 (c)
######################################################################################################################


###############################################################################
#For starting value senario One:initial value beta1=7, beta0=5 and lambda=0.4 #
###############################################################################

#Run the code for sample size N from 6000 to 300000 with increment 6000 and records every step for this scenario
MCsize=seq(6000,300000,by=6000)

u1_hat=rep(0,50)
u1_se=rep(0,50)

u0_hat=rep(0,50)
u0_se=rep(0,50)

lambda_hat=rep(0,50)
lambda_se=rep(0,50)

#Set initial values
beta10=7
beta11=7
beta00=5
beta01=5
lambda0=0.4
lambda1=0.4
beta0=beta00
beta1=beta10
lambda=lambda0
N=6000

for (i in 1:50)
{
  for (j in 1:N)
  {
    
    #Update beta1
    #Get distribution density of Y3 given beta10
    u1=beta00+beta10*X3
    dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta10, 0.7)
    beta11=rnorm(1,beta10,0.7)
    
    #Get distribution density of Y3 given beta11
    u2=beta00+beta11*X3
    dY12=dexpgauss(Y3,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta11,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1,r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta10=beta11
    }
    beta1=c(beta1,beta10)
    
    
    #Update beta0
    #Get distribution density of Y3 given beta00
    u1=beta00+beta10*X3
    dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta00, 0.7)
    beta01=rnorm(1,beta00,0.7)
    
    #Get distribution density of Y3 given beta11
    u2=beta01+beta10*X3
    dY12=dexpgauss(Y3,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta01,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta00=beta01
    }
    beta0=c(beta0,beta00)
    
    #Update lambda
    #Get distribution density of Y3 given lambda0
    u1=beta00+beta10*X3
    dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate lambda1 from proposal U(max(0,lambda0-0.3),lambda0+0.3)
    lambda1=runif(1,max(0,lambda0-0.3),lambda0+0.3)
    
    #Get distribution density of Y3 given lambda1
    dY12=dexpgauss(Y3,u1,sigma = 1, lambda = lambda1, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda1,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      lambda0=lambda1
    }
    lambda=c(lambda,lambda0)
    
  }
  
  #Get estimate of the posterior expectation of beta1, beta0, lambda and their standard errors  
  u1_hat[i]=bm(beta1)$est
  u1_se[i]=bm(beta1)$se
  u0_hat[i]=bm(beta0)$est
  u0_se[i]=bm(beta0)$se 
  lambda_hat[i]=bm(lambda)$est
  lambda_se[i]=bm(lambda)$se
}

#Record MCMC sample for all the parameters of this scenario
beta0_0=beta0
beta1_0=beta1
lambda_0=lambda

###############################################################################
#For starting value senario Two:initial value beta1=4, beta0=3 and lambda=0.3 #
###############################################################################

#Run the code for sample size N from 6000 to 300000 with increment 6000 and records every step for this scenario
MCsize=seq(6000,300000,by=6000)

u1_hat1=rep(0,50)
u1_se1=rep(0,50)

u0_hat1=rep(0,50)
u0_se1=rep(0,50)

lambda_hat1=rep(0,50)
lambda_se1=rep(0,50)

#Set initial values
beta10=4
beta11=4
beta00=3
beta01=3
lambda0=0.3
lambda1=0.3
beta0=beta00
beta1=beta10
lambda=lambda0
N=6000

for (i in 1:50)
{
  for (j in 1:N)
  {
    
    #Update beta1
    #Get distribution density of Y3 given beta10
    u1=beta00+beta10*X3
    dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta10, 0.7)
    beta11=rnorm(1,beta10,0.7)
    
    #Get distribution density of Y3 given beta11
    u2=beta00+beta11*X3
    dY12=dexpgauss(Y3,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta11,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1,r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta10=beta11
    }
    beta1=c(beta1,beta10)
    
    
    #Update beta0
    #Get distribution density of Y3 given beta00
    u1=beta00+beta10*X3
    dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta00, 0.7)
    beta01=rnorm(1,beta00,0.7)
    
    #Get distribution density of Y3 given beta11
    u2=beta01+beta10*X3
    dY12=dexpgauss(Y3,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta01,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta00=beta01
    }
    beta0=c(beta0,beta00)
    
    #Update lambda
    #Get distribution density of Y3 given lambda0
    u1=beta00+beta10*X3
    dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate lambda1 from proposal U(max(0,lambda0-0.3),lambda0+0.3)
    lambda1=runif(1,max(0,lambda0-0.3),lambda0+0.3)
    
    #Get distribution density of Y3 given lambda1
    dY12=dexpgauss(Y3,u1,sigma = 1, lambda = lambda1, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda1,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      lambda0=lambda1
    }
    lambda=c(lambda,lambda0)
    
  }
  
  #Get estimate of the posterior expectation of beta1, beta0, lambda and their standard errors  
  u1_hat1[i]=bm(beta1)$est
  u1_se1[i]=bm(beta1)$se
  u0_hat1[i]=bm(beta0)$est
  u0_se1[i]=bm(beta0)$se 
  lambda_hat1[i]=bm(lambda)$est
  lambda_se1[i]=bm(lambda)$se
}

#Record MCMC sample for all the parameters of this scenario
beta0_1=beta0
beta1_1=beta1
lambda_1=lambda

################################################################################
#For starting value senario Three:initial value beta1=1,beta0=1 and lambda=0.2 #
################################################################################

#Run the code for sample size N from 6000 to 300000 with increment 6000 and records every step for this scenario
MCsize=seq(6000,300000,by=6000)

u1_hat2=rep(0,50)
u1_se2=rep(0,50)

u0_hat2=rep(0,50)
u0_se2=rep(0,50)

lambda_hat2=rep(0,50)
lambda_se2=rep(0,50)

#Set initial values
beta10=1
beta11=1
beta00=1
beta01=1
lambda0=0.2
lambda1=0.2
beta0=beta00
beta1=beta10
lambda=lambda0
N=6000

for (i in 1:50)
{
  for (j in 1:N)
  {
    
    #Update beta1
    #Get distribution density of Y3 given beta10
    u1=beta00+beta10*X3
    dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta10, 0.7)
    beta11=rnorm(1,beta10,0.7)
    
    #Get distribution density of Y3 given beta11
    u2=beta00+beta11*X3
    dY12=dexpgauss(Y3,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta11,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1,r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta10=beta11
    }
    beta1=c(beta1,beta10)
    
    
    #Update beta0
    #Get distribution density of Y3 given beta00
    u1=beta00+beta10*X3
    dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate beta11 from proposal N(beta00, 0.7)
    beta01=rnorm(1,beta00,0.7)
    
    #Get distribution density of Y3 given beta11
    u2=beta01+beta10*X3
    dY12=dexpgauss(Y3,u2,sigma = 1, lambda = lambda0, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta01,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      beta00=beta01
    }
    beta0=c(beta0,beta00)
    
    #Update lambda
    #Get distribution density of Y3 given lambda0
    u1=beta00+beta10*X3
    dY11=dexpgauss(Y3,u1,sigma = 1, lambda = lambda0, log = FALSE)
    f1=sum(log(dY11))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda0,0.01,0.01))
    
    #Generate lambda1 from proposal U(max(0,lambda0-0.3),lambda0+0.3)
    lambda1=runif(1,max(0,lambda0-0.3),lambda0+0.3)
    
    #Get distribution density of Y3 given lambda1
    dY12=dexpgauss(Y3,u1,sigma = 1, lambda = lambda1, log = FALSE)
    f2=sum(log(dY12))+log(dnorm(beta10,0,10))+log(dnorm(beta00,0,10))+log(dgamma(lambda1,0.01,0.01))
    
    r=exp(f2-f1)
    #Get acceptance probability
    alpha=min(1, r)
    
    #Reject or Accept the Proposal and get a new sample
    uu=runif(1,0,1)
    if (uu<alpha)
    {
      lambda0=lambda1
    }
    lambda=c(lambda,lambda0)
    
  }
  
  #Get estimate of the posterior expectation of beta1, beta0, lambda and their standard errors  
  u1_hat2[i]=bm(beta1)$est
  u1_se2[i]=bm(beta1)$se
  u0_hat2[i]=bm(beta0)$est
  u0_se2[i]=bm(beta0)$se 
  lambda_hat2[i]=bm(lambda)$est
  lambda_se2[i]=bm(lambda)$se
}

#Record MCMC sample for all the parameters of this scenario
beta0_2=beta0
beta1_2=beta1
lambda_2=lambda

#Plot support plots to show how accurate my approach is
par(mfrow=c(3,4))

#Support plots of beta0
plot(density(beta0_0),main="Density Plot of Beta0")
points(density(beta0_1),type="l",col="red")
points(density(beta0_2),type="l",col="green")
plot(MCsize,u0_hat,type="l",ylim=c(0.13,0.17),main="Estimate of Beta0 vs Sample Size")
points(MCsize,u0_hat1,type="l",col="red")
points(MCsize,u0_hat2,type="l",col="green")
plot(MCsize,u0_se,type="l",main="SE of Beta0 vs Sample Size")
points(MCsize,u0_se1,type="l",col="red")
points(MCsize,u0_se2,type="l",col="green")
acf(beta0_0,main="Acf of MCMC Sample Beta0 ")

#Give estimate effective sample sizes of beta0
ess(beta0_0)
ess(beta0_1)
ess(beta0_2)


#Support plots of beta1
plot(density(beta1_0),main="Density Plot of Beta1")
points(density(beta1_1),type="l",col="red")
points(density(beta1_2),type="l",col="green")
plot(MCsize,u1_hat,type="l",ylim=c(2.42,2.52),main="Estimate of Beta1 vs Sample Size")
points(MCsize,u1_hat1,type="l",col="red")
points(MCsize,u1_hat2,type="l",col="green")
plot(MCsize,u1_se,type="l",main="SE of Beta1 vs Sample Size")
points(MCsize,u1_se1,type="l",col="red")
points(MCsize,u1_se2,type="l",col="green")
acf(beta1_0,main="Acf of MCMC Sample Beta0 ")

#Give estimate effective sample sizes of beta1
ess(beta1_0)
ess(beta1_1)
ess(beta1_2)


#Support plots of lambda
plot(density(lambda_0),main="Density Plot of lambda")
points(density(lambda_1),type="l",col="red")
points(density(lambda_2),type="l",col="green")
plot(MCsize,lambda_hat,type="l",main="Estimate of Lambda vs Sample Size")
points(MCsize,lambda_hat1,type="l",col="red")
points(MCsize,lambda_hat2,type="l",col="green")
plot(MCsize,lambda_se,type="l",main="SE of Lambda vs Sample Size")
points(MCsize,lambda_se1,type="l",col="red")
points(MCsize,lambda_se2,type="l",col="green")
acf(lambda_0,main="Acf of MCMC Sample lambda ")

#Give estimate effective sample sizes of lambda
ess(lambda_0)
ess(lambda_1)
ess(lambda_2)


