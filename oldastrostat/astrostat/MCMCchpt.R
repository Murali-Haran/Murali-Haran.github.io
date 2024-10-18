## Markov chain Monte Carlo algorithm for a Bayesian (single) change point model
## read in the data
##chptdat <- read.table("chpt.dat",header=T)
#Y <- chptdat$Ener
#n <- length(Y)

#chptdat <- read.table("coal.dat",header=T)
#Y <- chptdat$Deaths

mhsampler <- function(NUMIT=500,dat=Y)
  {
    n <- length(Y)
    ## set up
    ## NUMIT x 5 matrix to store Markov chain values
    ## each row corresponds to one of 5 parameters in order: theta,lambda,k,b1,b2
    ## each column corresponds to a single state of the Markov chain
    mchain <- matrix(NA, 5, NUMIT)
    
    ## starting values for Markov chain
    ## This is somewhat arbitrary but any method that produces reasonable values for each parameter is usually adequate.
    ## For instance, we can use approximate prior means or approximate MLEs.
    
    kinit <- floor(n/2) # approximately halfway between 1 and n
    mchain[,1] <- c(1,1,kinit,1,1)
    
    for (i in 2:NUMIT)
      {
        ## sample from full conditional distribution of theta (Gibbs update)
        newtheta <- rgamma(1,shape=sum(Y[1:mchain[3,i-1]])+0.5, scale=mchain[4,i-1]/(mchain[3,i-1]*mchain[4,i-1]+1))
        
        ## sample from full conditional distribution of lambda (Gibbs update)
        newlambda <- rgamma(1,shape=sum(Y[(mchain[3,i-1]+1):n])+0.5, scale=mchain[5,i-1]/(mchain[3,i-1]*mchain[5,i-1]+1))
        
        ## sample from full conditional distribution of k (Metropolis-Hastings update)
        newk <- 40
        ##    kprop <- sample(x=seq(1,n), size=1) # draw one sample at random from uniform{1,..n}
        
        ## sample from full conditional distribution of b1 (Gibbs update): draw from Inverse Gamma
        newb1 <- 1/rgamma(1,shape=0.5, scale=1/(mchain[1,i-1]+1))
        
        ## sample from full conditional distribution of b2 (Gibbs update): draw from Inverse Gamma
        newb2 <- 1/rgamma(1,shape=0.5, scale=1/(mchain[2,i-1]+1))
        
        ## update chain with new values
        mchain[,i] <- c(newtheta,newlambda,newk,newb1,newb2)
        
      }

    cat("Markov chain algorithm ran for ",NUMIT,"iterations \n")

    return(mchain)
  }
