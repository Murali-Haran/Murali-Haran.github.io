########################################################
## Simple linear regression
########################################################
regdat=read.table("http://personal.psu.edu/muh10/regdat.txt")



########################################################
## Exponentially Modified Gaussian Regression
########################################################
expregdat=read.table("http://personal.psu.edu/muh10/expregdat.txt")

erfc=function(x){
    2*pnorm(sqrt(2)*x) - 1
}
logerfc=function(x){
    log(2*pnorm(sqrt(2)*x) - 1)
}

## priors
logbeta=function(x){
    1
}
loglambda=function(x,a=1,b=10){
    (a-1)*log(x) -x/b
}
logtau=function(x,a=1,b=10){
    (a-1)*log(x) -x/b
}

TAU=2 # assumed known

## full conditionals
logbetaFC=function(beta,lambda, tau=TAU, Y=expregdat$Y,X=expregdat$X){
    logval1=sum(sapply((X*beta + lambda*tau - Y)/(sqrt(2)*tau), logerfc))
    logval2=(lambda/2)*sum(2*X*beta + lambda*tau - 2*Y)
    logval=logval1 + logval2 + logbeta(beta) 
}

####################################
## M-H Algorithm
## start off by assuming lambda is
## also known, only estimate beta
## let lambda = 10
LAMBDA=10
####################################
mh=function(NUMIT=10000,betaInit=5,lambdaInit=LAMBDA){
    mchain=matrix(NA,NUMIT,2)
    mchain[1,]=c(betaInit, lambdaInit) # initial values
    mchain[,2] = LAMBDA # set all lambdas to LAMBDA
    for (i in 2:NUMIT){
        ## M-H update for beta
        propbeta=rnorm(1,mchain[i-1,1],0.1)
        U=runif(1)
        if (logbetaFC(propbeta, mchain[i-1,2])-logbetaFC(mchain[i-1,1], mchain[i-1,2]) < log(U))
            mchain[i,1]=propbeta # accept
        else
            mchain[i,1]=mchain[i-1,1] # reject
        }
        ## M-H update for lambda
}

set.seed(1)
mcmc= mh(100)
