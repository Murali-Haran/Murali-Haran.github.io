########################################################
## Simple linear regression
########################################################
regdat=read.table("http://personal.psu.edu/muh10/regdat.txt")

## priors
logbeta=function(x){
    1
}

logtau=function(x,a=1,b=10){
    (a-1)*log(x) -x/b
}


## full conditionals
logbetaFC=function(beta,tau, Y=regdat$Y,X=regdat$X){
    n=length(regdat$Y)
    logval1=-0.5*n*log(tau) - (0.5/tau)*sum((X*beta - Y)^2)
    logval=logval1 + logbeta(beta) 
}

logtauFC=function(beta,tau, Y=regdat$Y,X=regdat$X){
    if ((tau<10) && (tau>2))
        {
            n=length(regdat$Y)
            logval1=-0.5*n*log(tau) - (0.5/tau)*sum((X*beta - Y)^2)
            logval=logval1 + logbeta(beta)
        }
    else
        logval=-Inf

    return(logval)
}

####################################
## M-H Algorithm
## checking code: good to start off by assuming tau is 
## known, only estimate beta e.g. let tau = 10
####################################
mh=function(NUMIT=10000,betaInit=5,tauInit=3){
    mchain=matrix(NA,NUMIT,2)
    mchain[1,]=c(betaInit, tauInit) # initial values
    for (i in 2:NUMIT){
        ## M-H update for beta
        propbeta=rnorm(1,mchain[i-1,1],0.1)
        U=runif(1)
        if (logbetaFC(propbeta, mchain[i-1,2])-logbetaFC(mchain[i-1,1], mchain[i-1,2]) < log(U))
            mchain[i,1]=propbeta # accept
        else
            mchain[i,1]=mchain[i-1,1] # reject
        }
    ## M-H update for tau
    proptau=rnorm(1,mchain[i-1,2],0.1)
    U=runif(1)
    if (logtauFC(mchain[i,1],proptau)-logbetaFC(mchain[i,1], mchain[i-1,2]) < log(U))
        mchain[i,1]=propbeta # accept
    else
        mchain[i,1]=mchain[i-1,1] # reject

}

set.seed(1)
mcmc= mh(3)
