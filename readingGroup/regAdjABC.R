##################################################
## toy example for regression adjusted ABC
## Beaumont et al. (2002)
## June 19, 2013
##################################################

## suppose we want to sample from posterior
## for model Y|theta ~ Binom(n=20, theta)
## with prior: p(theta) is Unif(0,1)
## Suppose observation is generated as follows
set.seed(1)
n<-20
Y <- rbinom(1,n,0.2) # Y is 3

## Step 1: simulate from (Y,theta) using above model
M <- 10000 # Monte Carlo sample size
thetaPriorSamp <- runif(M, 0,1)
YSamp <- rep(NA,M)
for (i in 1:M)
  YSamp[i] <- rbinom(1,n,thetaPriorSamp[i])

## Step 2: fit regression
diffY <-  YSamp-Y
regFit <- lm(thetaPriorSamp ~ diffY)
alphaHat <-regFit$coeff[1]
betaHat <-regFit$coeff[2]
plot(YSamp,thetaPriorSamp)
  
## Step 3: obtain reg-adjusted ABC posterior samples
thetaPosteriorSamp <- thetaPriorSamp - betaHat*(YSamp - Y)
plot(density(thetaPosteriorSamp, xlim=c(0,1)),col="red",main="Reg-adjusted ABC samples in red, true posterior in black")

## true posterior is Beta(Y+1, n-Y+1)
thetaTruePost <- rbeta(M, Y+1, n-Y+1)
lines(density(thetaTruePost))

