####
#Question 1
####

set.seed(312)

data.q1 <- read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
xs <- data.q1 [,1]
ys <- data.q1 [,2]
beta0 <- 5
lambda <- 0.4
sigma <- 1
m <- seq(100, 100000, by=100)


####
#In writing and optimizing the log likelihood,
#I have directly used the pnorm function,
#which evaluates the erfc() function value.
##
##Also, terms that are constants
#with respect to beta1 are ignored
#### 

log.likelihood.q1 <- function (b1) {
  sum (lambda * b1 * xs) +
    sum (log (1 - pnorm ((beta0 + b1*xs + lambda*sigma^2 - ys)/(sqrt(2) * sigma))))
}


####
#Below plot idenfies the MLE of beta1.
#I looked at plots over a much larger range
#(-300000, 300000) to ensure that
#this is the global maximum.
##
##I think the mean value theorem
#must also hold in this case
##
##This value will be used as starting value
#in the M-H algorithm.
####

be1 <- seq(-5, 15, length.out = 100000)
plot(be1, sapply(be1, log.likelihood.q1),
     main="Log-likelihood: vline at MLE beta1",
     xlab="beta1 values", ylab="log-likelihood"
)
abline(v = be1[which.max(sapply(be1, log.likelihood.q1))],
       col="red", lwd=3
)

mle.beta1 <- be1[which.max(sapply(be1, log.likelihood.q1))]


####
##Now, we write a function for posterior
#of beta1, upto a normalizing constant,
#where the prior is a normal distribution
#with mean = 0, and SD = 10.
##
##We adjust for a constant inside the exponant.
##
##This constant is calculated to be
#the maximum of the terms inside the exponent,
#within the possible range for beta1 from -Inf to Inf.
##
##The maximum can be verified using the commented code
#in the line below. I have verified this using 
#larger ranges.
##
##max((lambda * be1 * sum(xs)) - be1^2)
##
##First use the MLE as initial value
##
##Graphical evaluation of the plot of posterior
#indicates the centre to be around 6. Therefore,
#6 will be used as the second starting value.
##
##As a third proposed starting value, we will use
#a value in between the other two i.e. 7
####

tau <- 0.5
initial1 <- mle.beta1
initial2 <- 6
initial3 <- 7

h.beta1 <- function (b1) {
  exp((lambda * b1 * sum(xs)) - b1^2 - 107) *
    (prod (1 - pnorm(((beta0 + b1*xs + lambda*sigma^2 - ys)/(sqrt(2) * sigma)))))
}

plot(be1, sapply(be1, h.beta1), main="Posterior of beta1",
     xlab="beta1 values", ylab="Posterior density"
)

lines(be1, dnorm(be1, initial1, tau), col="red", lwd=4)
lines(be1, dnorm(be1, initial2, tau), col="blue", lwd=4)
lines(be1, dnorm(be1, initial3, tau), col="green", lwd=4)


####
##Based on graphical evaluation, we see that
#both initial values would require two different
##tuning parameters, since the centres are at
#different distance from the posterior centre.
#However, a tau value of 0.5 performs better than 0.2
#on all fronts. Hence that is the one used
##
##We will be using multiple codes from the batchmenas file,
#hence sourcing it here
####

source("http://www.stat.psu.edu/~mharan/batchmeans.R")


####
##We will now build a random walk M-H algorithm.
##
##Since we are using a random walk M-H, q(x) is symmetric.
#Therefore we have to only keep track of the posterior
####

MH.sampling <- function(n, sample0, tau){
  
  #We set initial value once for every n
  sample <- initial1
  while(length(sample) < n){
    
    #For each new proposal, we generate uniform
    #to compare it with
    u <- runif(1,0,1)
    
    #We generate the next sample using normal
    #with previous sample as the mean
    new.sample <- rnorm(1,sample[length(sample)],tau)
    
    #Below we run the code that determines whether
    #to accept a new sample or not
    
    if ((h.beta1(new.sample))==0){
      sample <- c(sample,new.sample)
    } else if(u < min(1,h.beta1(new.sample)/h.beta1(sample[length(sample)]))){
      sample <- c(sample,new.sample)
    } else{
      sample <- c(sample, sample[length(sample)])	
    }
  }
  list(n=n, sample=sample, exp.var=bm(sample),
       acf=acf(sample, main=paste("ACF for n=",n)),
       accept.rate=length(unique(sample))/length(sample),
       ess = ess(sample))
}

####
##We will see results of the M-H algorithm using all three
#starting values, with tuning parameter 0.5.
##
##The list that saves the output and various features
#of the M-H algorithm is called 'a.bunch.of.things'
#as it literally contains a bunch of things.
####

a.bunch.of.things1 <- sapply(m, MH.sampling, sample0=initial1, tau=tau)
a.bunch.of.things2 <- sapply(m, MH.sampling, sample0=initial2, tau=tau)
a.bunch.of.things3 <- sapply(m, MH.sampling, sample0=initial3, tau=tau)


####
##In order to decide whether this is a good algorithm,
#we will look at the plots that were discussed in class.
##
##Every plot observed for all combinations.
####

estimates1 <- rep(NA, length(m))
for (i in 1:length(m)){
  estimates1[i] <- as.numeric(a.bunch.of.things1[3,][[i]][1])
}

MCMCse1 <- rep(NA, length(m))
for (i in 1:length(m)){
  MCMCse1[i] <- as.numeric(a.bunch.of.things1[3,][[i]][2])
}


estimates2 <- rep(NA, length(m))
for (i in 1:length(m)){
  estimates2[i] <- as.numeric(a.bunch.of.things2[3,][[i]][1])
}

MCMCse2 <- rep(NA, length(m))
for (i in 1:length(m)){
  MCMCse2[i] <- as.numeric(a.bunch.of.things2[3,][[i]][2])
}


estimates3 <- rep(NA, length(m))
for (i in 1:length(m)){
  estimates3[i] <- as.numeric(a.bunch.of.things3[3,][[i]][1])
}

MCMCse3 <- rep(NA, length(m))
for (i in 1:length(m)){
  MCMCse3[i] <- as.numeric(a.bunch.of.things3[3,][[i]][2])
}


#Begin plots:
par(mfrow=c(3,4))

plot(m, MCMCse1, xlab="Sample size", ylab="MCMCse", main="MCMCse: MLE initial")
plot(m, a.bunch.of.things1[6,], xlab="Sample size", ylab="ESS", main="ESS: MLE initial")
plot(m, a.bunch.of.things1[5,], xlab="Sample size", ylab="Acceptance rate", main="Accept rate: MLE initial")
acf(unlist(a.bunch.of.things1[2,length(m)]), main="ACF: MLE initial")

plot(m, MCMCse2, xlab="Sample size", ylab="MCMCse", main="MCMCse: initial val 6")
plot(m, a.bunch.of.things2[6,], xlab="Sample size", ylab="ESS", main="ESS: initial val 6")
plot(m, a.bunch.of.things2[5,], xlab="Sample size", ylab="Acceptance rate", main="Accept rate: initial val 6")
acf(unlist(a.bunch.of.things2[2,length(m)]), main="ACF: initial val 6")

plot(m, MCMCse2, xlab="Sample size", ylab="MCMCse", main="MCMCse: initial val 7")
plot(m, a.bunch.of.things3[6,], xlab="Sample size", ylab="ESS", main="ESS: initial val 7")
plot(m, a.bunch.of.things3[5,], xlab="Sample size", ylab="Acceptance rate", main="Accept rate: initial val 7")
acf(unlist(a.bunch.of.things3[2,length(m)]), main="ACF: initial val 7")

par(mfrow=c(1,1))

plot(density(estimates1), xlab="Estimate", main="Density plots of estimates", col="red", lwd=3, ylim=c(0,40))
legend('topright', c("MLE initial","Initial 6", "Initial 7"),
       lty=1, lwd=2, col=c("Red", "Blue", "Green"), bty="o")
lines(density(estimates2), col="blue", lwd=3)
lines(density(estimates3), col="green", lwd=3)


####
##Since these plots indicate initial value = 7
#to be the best option among all, we report
#various measures based on the last sample
#i.e. n=10000, using the initial value 7
####

expected.beta1 <- estimates3[length(m)]
MCMCse <- MCMCse3[length(m)]
cred.int <- quantile(unlist(a.bunch.of.things3[2,length(m)]), c(0.025, 0.975))
plot(density (unlist(a.bunch.of.things3[2,length(m)])),
     main="Final posterior density of beta1",
     lwd=4, col="dodgerblue4"
)
cat("Estimated mean=", expected.beta1, ",", "MCMCse=", MCMCse, ",", "Credible interval=", "(",cred.int,")")

####
rm(list = ls())
####




####
#Problem 2
####

set.seed(231)

data.q2 <- read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
xs <- data.q2 [,1]
ys <- data.q2 [,2]
sigma <- 1
n <- length(xs)
m <- seq(100, 50000, by=200)

source("http://sites.stat.psu.edu/~mharan/515//hwdir/emg.R")
source("http://sites.stat.psu.edu/~mharan/MCMCtut/MCMCchpt.R")


####
##Write the posterior of each parameter,
#upto a normalizing constant. Same as the
#full conditional distribution
####

h.beta0 <- function(b0, b1, lam){
  exp(-b0^2 + (200 * n * lam * b0)) *
    (prod (1 - pnorm((b0 + b1*xs + lam*sigma^2 - ys)/(sqrt(2) * sigma))))
}

h.beta1 <- function(b0, b1, lam){
  exp(-b1^2 + (200 * lam * b1 * sum(xs))) *
    (prod (1 - pnorm((b0 + b1*xs + lam*sigma^2 - ys)/(sqrt(2) * sigma))))
}

h.lambda <- function(b0, b1, lam){
  lam^0.99 * exp((lam * (100*n - 1 + 100*n*b0 + 100*b1*sum(xs))) / 100) *
    (prod (1 - pnorm((b0 + b1*xs + lam*sigma^2 - ys)/(sqrt(2) * sigma))))
}


####
##We will now write the code for the
#one-variable-at-a-time M-H algorithm.
##
##For the parameters that need Gibbs sampling,
#we will simply generate a value and add
#it to the vector. For the third parameter,
#we propose a random walk or a normal proposal.
#Therefore, we will use the standard M-H sampling
#idea for updating lambdas.
####

MH.algorithm <- function(m, initial.b0, initial.b1, initial.lam, tau){				
  #We set initial value once for every m
  sample <- data.frame(initial.b0, initial.b1, initial.lam)
  while(length(sample[,1]) < m){
    
    #For each new proposal, we generate uniform
    #to compare it with
    u <- runif(1,0,1)
    
    #We generate the next lambda sample using normal
    #with previous sample as the mean and betas using
    #the rexpgauss function
    new.sample.b0 <- rexpgauss(1, 100*n*sample[length(sample[,1]) ,3], 10, sample[length(sample[,1]),3])
    new.sample.b1 <- rexpgauss(1, 100*sample[length(sample[,1]) ,3]*sum(xs), 10, sample[length(sample[,1]),3])
    new.sample.lam <- rnorm(1, sample[length(sample[,1]) ,3], tau)
    new.sample <- c(new.sample.b0, new.sample.b1, new.sample.lam)
    
    #Below we run the code that determines whether
    #to accept a new sample for lambda or not
    
    if ((h.lambda(sample[length(sample[,1]),1], sample[length(sample[,1]),2], new.sample.lam))==0){
      sample <- rbind(sample,new.sample)
    } else if(u < min(1,h.lambda(sample[length(sample[,1]) ,1], sample[length(sample[,1]) ,2], new.sample.lam)/h.lambda(sample[length(sample[,1]) ,1], sample[length(sample[,1]) ,2] ,sample[length(sample[,1]),3]))){
      sample <- rbind(sample,new.sample)
    } else{
      sample <- rbind(sample, sample[length(sample[,1]),])	
    }
  }
  list(m=m, sample=sample, exp.var=bm(sample),
       acf=acf(sample, main=paste("ACF for m=",m)),
       accept.rate=length(unique(sample))/length(sample),
       ess = ess(sample))
}

a.bunch.of.things <- sapply(m, MH.algorithm, initial.b0=6, initial.b1=8, initial.lam=0.5, tau=0.5)

