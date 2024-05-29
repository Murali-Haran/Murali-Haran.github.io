## Markov chain Monte Carlo algorithm for a Bayesian (single) change point model
NUMIT <- 500 # number of iterations of sampler (Markov chain length)

## read in the data
chptdat <- read.table("chptdat",header=T)
Y <- chptdat$Ener
n <- length(Y)

## set up
## NUMIT x 5 matrix to store Markov chain values
## each row corresponds to one of 5 parameters in order: theta,lambda,k,b1,b2
## each column corresponds to a single state of the Markov chain
thetachain <- rep(NA, NUMIT)
lambdachain <- rep(NA, NUMIT)
kchain <- rep(NA, NUMIT)
b1chain <- rep(NA, NUMIT)
b2chain <- rep(NA, NUMIT)

## starting values for Markov chain
## This is somewhat arbitrary but any method that produces reasonable values for each parameter is usually adequate.
## For instance, we can use approximate prior means or approximate MLEs.

kinit <- floor(n/2) # approximately halfway between 1 and n
thetachain[1] <- 1
lambdachain[1] <- 1
kchain[1] <- kinit
b1chain[1] <- 1
b2chain[1] <- 1

mchain[,1] <- c(1,1,kinit,1,1)
                
for (i in 2:NUMIT)
  {
    ## sample from full conditional distribution of theta (Gibbs update)
    newtheta <- rgamma(shape=sum(Y[1:kchain[i-1]])+0.5, scale=b1chain[i-1]/(kchain[i-1]*b1chain[i-1]+1))
    
    ## sample from full conditional distribution of lambda (Gibbs update)
    newlambda <- rgamma(shape=sum(Y[(kchain[i-1]+1):n])+0.5, scale=b1chain[i-1]/(kchain[i-1]*b1chain[i-1]+1))
    
    ## sample from full conditional distribution of k (Metropolis-Hastings update)
    newk <- 40
    ##    kprop <- sample(x=seq(1,n), size=1) # draw one sample at random from uniform{1,..n}
    
    ## sample from full conditional distribution of b1 (Gibbs update): draw from Inverse Gamma
    newb1 <- 1/rgamma(shape=0.5, scale=1/(thetachain[i-1]+1))

    ## sample from full conditional distribution of b2 (Gibbs update): draw from Inverse Gamma
    newb2 <- 1/rgamma(shape=0.5, scale=1/(lambdachain[i-1]+1))

    ## update chain with new values
    thetachain[i] <- newtheta
    lambdachain[i] <- newlambda
    kchain[i] <- newk
    b1chain[i] <- newb1
    b2chain[i] <- newb2
    
  }
