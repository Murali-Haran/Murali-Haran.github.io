#########################################
##
##Question 1
##
#########################################

#NOTE: code commenting is mainly done in Question 1 as
# there was much code repeated in subsequent questions.
# However, I try to take note of changes made in later questions.


data1 <- read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
colnames(data1) <- c("X", "Y")
source("http://www.stat.psu.edu/~mharan/batchmeans.R")
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")
# install.packages("mcmcplots")
# install.packages(("coda"))
# library("mcmcplots")
# library("coda")

#given in prompt
beta.0 = 5
lambda = 0.4
sigma.i = 1 #for all i

#let's look at it!
beta.1 = 8
mu=beta.0+beta.1*mean(data1$X)

plotdexpgauss(lowerlim = -10,upperlim = 20, mu = mu, sigma = 1, lambda = .4, log = F)

ts.plot(data1$Y, main="Time Series Plot of the Data")
### 
## Part a
###

#prior density ~ N(0,sd=10) Note the use of log. 
prior = function(beta.1){
  #prior density given in prompt
  beta.1.prior = dnorm(x = beta.1, mean = 0,sd = 10, log = T)
}

#likelihood   

likelihood = function(beta.1){
  #likelihood function found on log scale. joint density found by summing all log densities (instead of multiplying)
  mu = beta.0+beta.1*mean(data1$X)
  singlelikelihoods = dexpgauss(x = data1$Y,mu = mu, sigma = sigma.i, lambda = lambda,log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

#posterior density approximation 
posterior <- function(param){
  #approximating the posterior using the calulated likelihood + prior (again, adding because log scale)
  return (likelihood(param) + prior(param))
}

#proposal function - random walk normal - a asymmetric proposal was 
# considered, but i decided to stick with a more general version, because
# either way I would've been really paranoid about my choice and more example 
# code is available with this proposal. 

proposalfunction <- function(param, tau){
  return(rnorm(1,mean = param, sd= tau))
}


#MH Alg
#Goal: Approximate the posterior dist pi(beta.1|Y.vector, X)

run_metropolis_MCMC <- function(startvalue, iterations, tau){

  #create a counter for use in acceptance rate
  acc=0
  #create a safe, loving home the the chain
  chain = array(dim = c(iterations+1,1))
  
  #everyone has to start somewhere. our chain starts here.
  chain[1,] = startvalue
  
  for (i in 1:iterations){
    #a sweet, romantic proposal of beta.1^* 
    proposal = proposalfunction(chain[i,], tau)
    
    #what are the chances they'll say yes? remember everything is log
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      #they accept!
      acc = acc + 1
      #add new values for next state of the chain
      chain[i+1,] = proposal
    }else{
      #whomp whomp. keep the old values.
      chain[i+1,] = chain[i,]
    }
  }
  #let's have the function tell us what's happening
  cat("Markov chain algorithm ran for ",iterations,"iterations (acc.rate for beta.1=",acc/(iterations-1),")\n")
  return(chain)
} 

#start value based on previous runs
start = 5
#big, but not too big. juuuuuust right.
iterations = 10000


#pa-POW! let's check out a range of tau values for our proposal function
chain.001 = run_metropolis_MCMC(startvalue = start, iterations = iterations, tau = .001)
chain.01 = run_metropolis_MCMC(startvalue = start, iterations = iterations, tau = .01)
chain.1 = run_metropolis_MCMC(startvalue = start, iterations = iterations, tau = .1)
chain1 = run_metropolis_MCMC(startvalue = start, iterations = iterations, tau = 1)
chain2 = run_metropolis_MCMC(startvalue = start, iterations = iterations, tau = 2)

#   summary(chain.001)
#   plot(mcmc(chain.001))
#   mcmcplot1(chain.001)
#   
#   summary(chain.01)
#   plot(mcmc(chain.01))
#   mcmcplot1(chain.01) 

#summary(chain.1)
#plot(mcmc(chain.1))
#mcmcplot1(mcmc(chain.1)) 


summary(chain1) #take a peek at the chosen one! 
#plot(mcmc(chain1))
#mcmcplot1(mcmc(chain1)) 


#summary(chain2)
#plot(mcmc(chain2))
#mcmcplot1(chain2) 

##
# Part b
##

#Report posterior expectation of beta.1  

#bm(chain.001)
#bm(chain.01)
bm(chain.1)
bm(chain1)
bm(chain2)


###
## Part C
###

#Report a 95% credible interval for β1 based on your samples
# quantile(chain.001, c(0.025, 0.975))
# quantile(chain.01, c(0.025, 0.975))
 quantile(chain.1, c(0.025, 0.975))
# quantile(chain1, c(0.025, 0.975))
# quantile(chain2, c(0.025, 0.975))

###
## Part D
###
par(mfrow=c(1,1))
#Plot an estimate of the posterior pdf of β1 from a smoothed density plot of the samples.
# plot(density(chain.001), xlab="beta_1", main="Estimated Density")
# plot(density(chain.01), xlab="beta_1", main="Estimated Density")
# plot(density(chain.1), xlab="beta_1", main="Estimated Density")
 plot(density(chain1), xlab="beta_1", main="Estimated Density")
# plot(density(chain2), xlab="beta_1", main="Estimated Density")

###
## Part E
###

# determine that your approximations above were accurate

acf(chain.001)
acf(chain.01)
acf(chain.1)
acf(chain1)
acf(chain2)



ess(outp = chain.001,imselags = T)
ess(outp = chain.01,imselags = T)
ess(outp = chain.1,imselags = T)
ess(outp = chain1,imselags = T) # final used
ess(outp = chain2,imselags = T)

#estimates vs sample size - all taus plotted 
estPlot.001 = estvssamp(chain.001, retval = T)
estPlot.01 = estvssamp(chain.01, retval = T)
estPlot.1 = estvssamp(chain.1, retval = T)
estPlot1 = estvssamp(chain1, retval = T)
estPlot10 = estvssamp(chain2, retval = T)

par(mfrow=c(1,1))
plot(estPlot.001, ylim = c(4.5, 6.5), type="l", ylab= "beta.1 estimate", xlab = "Sample Size", main="Estimates versus sample size")

lines(estPlot.01, lty=2, col="red")
lines(estPlot.1, lty=2, col="blue")
lines(estPlot1, lty=2, col="green")
lines(estPlot10, lty=2, col="purple")


#######################################
##
## Question 2
##
#######################################
data2 <- read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
colnames(data2) <- c("X", "Y")

#known 
sigma.i = 1

#check it out
ts.plot(data2$Y, main="Time Series Plot of the Data")

#prior densitys ~ 
prior2 = function(param){
  beta.0 = param[1]
  beta.1 = param[2]
  lambda = param[3]
  
  beta.0.prior = dnorm(x = beta.0, mean = 0,sd = 10, log = T)
  beta.1.prior = dnorm(x = beta.1, mean = 0,sd = 10, log = T)
  lambda.prior = dgamma(x = lambda ,shape = .01 ,rate = .01,log = T) 
  
  return(beta.0.prior+beta.1.prior+lambda.prior)
}


#likelihood   

likelihood2 = function(param){
  beta.0 = param[1]
  beta.1 = param[2]
  lambda = param[3]
  
  mu=beta.0 + beta.1*data2$X
  singlelikelihoods = dexpgauss(x = data2$Y,mu = mu, sigma = sigma.i, lambda = lambda,log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

#posterior density approximation 
posterior2<- function(param){
  return (likelihood2(param) + prior2(param))
}


#proposal function - again normal random walk. 

proposalfunction2 <- function(param, tau){
  return(rnorm(3,mean = param, sd= tau ))
}


#MH Alg
#Goal: Approximate the posterior dist pi(beta.1|Y.vector, X)

run_metropolis_MCMC2 <- function(startvalues2, iterations, tau){
  
  chain2 = array(dim = c(iterations+1,3))
  colnames(chain2) = c("Beta.0", "Beta.1", "Lambda")
  chain2[1,] = startvalues2
  
  acc=0
  
  for (i in 1:iterations){
    proposal2 = proposalfunction2(chain2[i,], tau)
    
    probab2 = exp(posterior2(proposal2) - posterior2(chain2[i,]))
    if (runif(1) < probab2){
      acc = acc + 1
      chain2[i+1,] = proposal2
    }else{
      chain2[i+1,] = chain2[i,]
    }
  }
  cat("Markov chains algorithm ran for ",iterations,"iterations (acc.rate for all at once proposals is=",acc/(iterations-1),")\n")
  return(chain2)
}

start = c(2,3,1)
iterations = 20000

chain2.001 = run_metropolis_MCMC2(startvalue = start, iterations = iterations, tau = c(.001, .001, .001))
chain2.01 = run_metropolis_MCMC2(startvalue = start, iterations = iterations, tau = c(.01, .01, .01))
chain2..1 = run_metropolis_MCMC2(startvalue = start, iterations = iterations, tau = c(.1, .1, .1))
chain2.1 = run_metropolis_MCMC2(startvalue = start, iterations = iterations, tau = c(1, 1, .2))
chain2.10 = run_metropolis_MCMC2(startvalue = start, iterations = iterations, tau = c(10, 10, .1))

#summary(chain2.001)
#plot(mcmc(chain2.001))
#mcmcplot(chain2.001)

#summary(chain2.01)
#plot(mcmc(chain2.01))
#mcmcplot(chain2.01)

summary(chain2..1) #the chosen one
#plot(mcmc(chain2..1))
#mcmcplot(chain2..1)

#summary(chain2.1)
#plot(mcmc(chain2.1))
#mcmcplot(chain2.1) 

#summary(chain2.10)
#plot(mcmc(chain2.10))
#mcmcplot(chain2.10)



###
## Part B
###

#Provide, preferably in a well organized table, for beta.0,.beta.1,lambda, 
#the posterior mean w/ estimate of MCMC standard error in 
#parentheses, posterior 95% credible intervals.

bmmat(chain2.001)
bmmat(chain2.01)
bmmat(chain2..1) #chosen one
bmmat(chain2.1)
bmmat(chain2.10)  

quantile(chain2.001[,1], c(0.025, 0.975))
quantile(chain2.01[,1], c(0.025, 0.975))
quantile(chain2..1[,1], c(0.025, 0.975)) #final is this one
quantile(chain2.1[,1], c(0.025, 0.975))
quantile(chain2.10[,1], c(0.025, 0.975))

quantile(chain2.001[,2], c(0.025, 0.975))
quantile(chain2.01[,2], c(0.025, 0.975))
quantile(chain2..1[,2], c(0.025, 0.975)) #final used
quantile(chain2.1[,2], c(0.025, 0.975))
quantile(chain2.10[,2], c(0.025, 0.975))

quantile(chain2.001[,3], c(0.025, 0.975))
quantile(chain2.01[,3], c(0.025, 0.975))
quantile(chain2..1[,3], c(0.025, 0.975)) #final
quantile(chain2.1[,3], c(0.025, 0.975))
quantile(chain2.10[,3], c(0.025, 0.975))


estPlot2.001.1 = estvssamp(chain2.001[,1],retval = T)
estPlot2.01.1 = estvssamp(chain2.01[,1],retval = T)
estPlot2..1.1 = estvssamp(chain2..1[,1],retval = T)
estPlot2.1.1 = estvssamp(chain2.1[,1],retval = T)
estPlot2.10.1= estvssamp(chain2.10[,1],retval = T)

estPlot2.001.2 = estvssamp(chain2.001[,2],retval = T)
estPlot2.01.2 = estvssamp(chain2.01[,2],retval = T)
estPlot2..1.2 = estvssamp(chain2..1[,2],retval = T)
estPlot2.1.2 = estvssamp(chain2.1[,2],retval = T)
estPlot2.10.2= estvssamp(chain2.10[,2],retval = T)

estPlot2.001.3 = estvssamp(chain2.001[,3],retval = T)
estPlot2.01.3 = estvssamp(chain2.01[,3],retval = T)
estPlot2..1.3 = estvssamp(chain2..1[,3],retval = T)
estPlot2.1.3 = estvssamp(chain2.1[,3],retval = T)
estPlot2.10.3= estvssamp(chain2.10[,3],retval = T)

par(mfrow=c(3,1))

plot(estPlot2.001.1, ylim = c(1.5, 3.5), type="l", 
     main="Beta_0 Estimates versus sample size",
     xlab = "Sample Size", ylab = "beta_0")

lines(estPlot2.01.1, lty=2, col="red")
lines(estPlot2..1.1, lty=2, col="blue")
lines(estPlot2.1.1, lty=2, col="green")
lines(estPlot2.10.1, lty=2, col="purple")


plot(estPlot2.001.2, ylim = c(2, 4), type="l", 
     main="Beta_1 Estimates versus sample size",
     xlab = "Sample Size", ylab = "beta_1")

lines(estPlot2.01.2, lty=2, col="red")
lines(estPlot2..1.2, lty=2, col="blue")
lines(estPlot2.1.2, lty=2, col="green")
lines(estPlot2.10.2, lty=2, col="purple")


plot(estPlot2.001.3, ylim = c(0, 2), type="l", 
     main="Lambda Estimates versus sample size",
     xlab = "Sample Size", ylab = "lambda")

lines(estPlot2.01.3, lty=2, col="red")
lines(estPlot2..1.3, lty=2, col="blue")
lines(estPlot2.1.3, lty=2, col="green")
lines(estPlot2.10.3, lty=2, col="purple")




###
## Part C
### 

#Provide an approximation of the correlation between beta.0,beta.1.
#visualization of correlation, sadly not enough room for this plot
# in the report
pairs(chain2..1)


cor(chain2..1[,1], chain2..1[,2])


###
## Part D
###

#Provide approximate density plots for the marginal distributions 
#of beta.0, beta.1, lambda.

plot(density(chain2..1[,1]),main="Density for Beta_0 Posterior")
plot(density(chain2..1[,2]),main="Density for Beta_1 Posterior")
plot(density(chain2..1[,3]),main="Density for Lambda Posterior")

###
## Part E
###

#Determine how reliable your algorithm is
ess(outp = chain2.001,imselags = T)
ess(outp = chain2.01,imselags = T)
ess(outp = chain2..1,imselags = T) # final used
ess(outp = chain2.1,imselags = T) 
ess(outp = chain2.10,imselags = T)

#I actually used the mcmcplots for exporing acf. The values
#are higher than I'd prefer, but I am not a fan of sacrificing 
#information to lower correlation when the goal is estimation.
#However, if prompted, these (especially the betas) would 
#justify the use of thinning.
acf(chain2..1[,1],main="acf plot for beta.0")
acf(chain2..1[,2],main="acf plot for beta.1")
acf(chain2..1[,3],main="acf plot for lambda")


####################################################
##
## Question 3
##
####################################################
data3 <- read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")
colnames(data3) <- c("X", "Y")

#known 
sigma.i = 1

ts.plot(data3$Y, main="Time Series Plot of the Data")

#prior densitys ~ 
prior3 = function(param){
  beta.0 = param[1]
  beta.1 = param[2]
  lambda = param[3]
  
  beta.0.prior = dnorm(x = beta.0, mean = 0,sd = 10, log = T)
  beta.1.prior = dnorm(x = beta.1, mean = 0,sd = 10, log = T)
  lambda.prior = dgamma(x = lambda ,shape = .01 ,rate = .01,log = T) 
  
  return(beta.0.prior+beta.1.prior+lambda.prior)
}


#likelihood   

likelihood3 = function(param){
  beta.0 = param[1]
  beta.1 = param[2]
  lambda = param[3]
  
  mu=beta.0 + beta.1*data3$X
  singlelikelihoods = dexpgauss(x = data3$Y,mu = mu, sigma = sigma.i, lambda = lambda,log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

#posterior density approximation 
posterior3<- function(param){
  return (likelihood3(param) + prior3(param))
}


#proposal function - still the same. 


proposalfunction3 <- function(param, tau){
  return(rnorm(3,mean = param, sd= tau))
}

#MH Alg
#Goal: Approximate the posterior dist pi(beta.1|Y.vector, X)

run_metropolis_MCMC3 <- function(startvalues, iterations, tau){
  
  chain3 = array(dim = c(iterations+1,3))
  colnames(chain3) = c("Beta.0","Beta.1", "Lambda")
  chain3[1,] = startvalues
  
  acc = 0
  for (i in 1:iterations){
    proposal3 = proposalfunction3(chain3[i,], tau)
    
    probab3 = exp(posterior3(proposal3) - posterior3(chain3[i,]))
    if (runif(1) < probab3){
      acc = acc + 1
      chain3[i+1,] = proposal3
    }else{
      chain3[i+1,] = chain3[i,]
    }
  }
cat("Markov chains algorithm ran for ",iterations,"iterations (acc.rate for all at once proposals is=",acc/(iterations-1),")\n")

return(chain3)
}

start = c(0,2,.1)
iterations = 20000

chain3.001 = run_metropolis_MCMC3(startvalue = start, iterations = iterations, tau = c(.001, .001, .001))
chain3.01 = run_metropolis_MCMC3(startvalue = start, iterations = iterations, tau = c(.01, .01, .01))
chain3..1 = run_metropolis_MCMC3(startvalue = start, iterations = iterations, tau = c(.1, .1, .01))
chain3.1 = run_metropolis_MCMC3(startvalue = start, iterations = iterations, tau = c(1, 1, .01))
chain3.10 = run_metropolis_MCMC3(startvalue = start, iterations = iterations, tau = c(10, 10, .01))

#summary(chain3.001)
#plot(mcmc(chain3.001))
#mcmcplot(chain3.001)

#summary(chain3.01)
#plot(mcmc(chain3.01))
#mcmcplot(chain3.01)

summary(chain3..1) #the chosen one
#plot(mcmc(chain3..1))
#mcmcplot(chain3..1)

#summary(chain3.1)
#plot(mcmc(chain3.1))
#mcmcplot(chain3.1) 

#summary(chain3.10)
#plot(mcmc(chain3.10))
#mcmcplot(chain3.10)



###
## Part A
###

#Provide, preferably in a well organized table, for beta.0,.beta.1,lambda, 
#the posterior mean w/ estimate of MCMC standard error in 
#parentheses, posterior 95% credible intervals.
bmmat(chain3.001)
bmmat(chain3.01)
bmmat(chain3..1) #this one
bmmat(chain3.1)
bmmat(chain3.10)  

quantile(chain3.001[,1], c(0.025, 0.975))
quantile(chain3.01[,1], c(0.025, 0.975))
quantile(chain3..1[,1], c(0.025, 0.975)) #final is this one
quantile(chain3.1[,1], c(0.025, 0.975))
quantile(chain3.10[,1], c(0.025, 0.975))

quantile(chain3.001[,2], c(0.025, 0.975))
quantile(chain3.01[,2], c(0.025, 0.975))
quantile(chain3..1[,2], c(0.025, 0.975)) #final used
quantile(chain3.1[,2], c(0.025, 0.975))
quantile(chain3.10[,2], c(0.025, 0.975))

quantile(chain3.001[,3], c(0.025, 0.975))
quantile(chain3.01[,3], c(0.025, 0.975))
quantile(chain3..1[,3], c(0.025, 0.975)) #final
quantile(chain3.1[,3], c(0.025, 0.975))
quantile(chain3.10[,3], c(0.025, 0.975))

estPlot3.001.1 = estvssamp(chain3.001[,1],retval = T)
estPlot3.01.1 = estvssamp(chain3.01[,1],retval = T)
estPlot3..1.1 = estvssamp(chain3..1[,1],retval = T)
estPlot3.1.1 = estvssamp(chain3.1[,1],retval = T)
estPlot3.10.1= estvssamp(chain3.10[,1],retval = T)

estPlot3.001.2 = estvssamp(chain3.001[,2],retval = T)
estPlot3.01.2 = estvssamp(chain3.01[,2],retval = T)
estPlot3..1.2 = estvssamp(chain3..1[,2],retval = T)
estPlot3.1.2 = estvssamp(chain3.1[,2],retval = T)
estPlot3.10.2= estvssamp(chain3.10[,2],retval = T)

estPlot3.001.3 = estvssamp(chain3.001[,3],retval = T)
estPlot3.01.3 = estvssamp(chain3.01[,3],retval = T)
estPlot3..1.3 = estvssamp(chain3..1[,3],retval = T)
estPlot3.1.3 = estvssamp(chain3.1[,3],retval = T)
estPlot3.10.3= estvssamp(chain3.10[,3],retval = T)

par(mfrow=c(3,1))

plot(estPlot3.001.1, ylim = c(0, .5), type="l", 
     main="Beta_0 Estimates versus sample size",
     xlab = "Sample Size", ylab = "beta_1")
lines(estPlot3.01.1, lty=2, col="red")
lines(estPlot3..1.1, lty=2, col="blue")
lines(estPlot3.1.1, lty=2, col="green")
lines(estPlot3.10.1, lty=2, col="purple")


plot(estPlot3.001.2, ylim = c(1.5, 3), type="l", 
     main="Beta_1 Estimates versus sample size",
     xlab = "Sample Size", ylab = "beta_1")

lines(estPlot3.01.2, lty=2, col="red")
lines(estPlot3..1.2, lty=2, col="blue")
lines(estPlot3.1.2, lty=2, col="green")
lines(estPlot3.10.2, lty=2, col="purple")


plot(estPlot3.001.3, ylim = c(0, .5), type="l", 
     main="Lambda Estimates versus sample size",
     xlab = "Sample Size", ylab = "beta_1")

lines(estPlot3.01.3, lty=2, col="red")
lines(estPlot3..1.3, lty=2, col="blue")
lines(estPlot3.1.3, lty=2, col="green")
lines(estPlot3.10.3, lty=2, col="purple")


###
## Part b
###

#Provide approximate density plots for the marginal distributions 
#of beta.0, beta.1, lambda.
par(mfrow=c(1,3))
plot(density(chain3..1[,1]),main="Density for Beta_0 Posterior")
plot(density(chain3..1[,2]),main="Density  for Beta_1 Posterior")
plot(density(chain3..1[,3]),main="Density  for Lambda Posterior")


pairs(chain3..1)
cor(chain3..1[,1], chain3..1[,2])


