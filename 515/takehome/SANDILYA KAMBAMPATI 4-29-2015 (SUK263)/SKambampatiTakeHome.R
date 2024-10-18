rm(list=ls(all=TRUE))

# SELECT THE PROBLEM YOU WANT TO SOLVE. EXACTLY ONE OF THESE SHOULD BE TRUE
# Eg: IF you want to solve problem 3, set prob3 = TRUE and prob1 = prob2 = FALSE 
# Right now, the code is set to solve problem 3

prob1 = FALSE
prob2 = FALSE
prob3 = TRUE



if(prob1){
  ys = scan("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
}
if(prob2){
  ys = scan("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
}
if(prob3){
  ys = scan("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")
}


source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")

Ydat = ys[seq(2,200,2)]
Xdat = ys[seq(1,200,2)]


# ===============================================================================
#This function alpxy calculates the transition kernel for a given x, x* and data set 

#       INPUTS: 
# Ydat           Y data
# Xdat           X data
# beta0          beta0
# beta0s         beta0star
# beta1          beta1
# beta1s         beta1star
# lambda         lambda 
# lambdas        lambda star

# OUTPUT is the value of alpha(x,x*) 

alpxy = function(Ydat,Xdat,beta0,beta0s,beta1,beta1s,lambda,lambdas){
  
  
  if(prob1){ 
    #---------- These paramerters are for Problem 1. ---------------------
    varbeta0 = 100 
    sigi = 1
    #-------- These Parameters can be anything, as they get canceled out in Problem 1
    varbeta1 = 100                    
    mulambda = 1
    sdlambda = 10
    shape = mulambda^2/sdlambda^2
    rate = mulambda/sdlambda^2
    
    #---------------------------------------------------------------------------------
  }
  
  if(prob2 || prob3){ 
    #---------- These paramerters are for Problem 2 and 3 ---------------------
    varbeta0 = 100
    varbeta1 = 100
    mulambda = 1
    sdlambda = 10
    shape = mulambda^2/sdlambda^2
    rate = mulambda/sdlambda^2
    sigi = 1
    #---------------------------------------------------------------------------------
  }
  
  numer = -beta1s^2/2/varbeta1 -beta0s^2/2/varbeta0 + (shape-1)*log(lambdas) - rate*lambdas
  denom = -beta1^2/2/varbeta1 -beta0^2/2/varbeta0 + (shape-1)*log(lambda) - rate*lambda
  
  if(prob1){
    qlamnum = 1                                                     # For Problem 1
    qlamden = 1                                                     # For Problem 1
  }
  
  if(prob2){
    
    qlamnum = dgamma(lambda,lambdas^2,lambdas,log = 'true')           # For Problem 2
    qlamden = dgamma(lambdas,lambda^2,lambda,log = 'true')            # For Problem 2
    
  }
  
  if(prob3){
    
    qlamnum = dbeta(lambda,10,10*(1/lambda - 1),log = 'true')             # For Problem 3
    qlamden = dbeta(lambdas,10,10*(1/lambdas - 1),log = 'true')           # For Problem 3
  }
  
  
  
  for (i in 1:length(Ydat)){
    
    numer = numer + dexpgauss(Ydat[i],beta0s+beta1s*Xdat[i],sigi,lambdas,log = 'true')
    denom = denom + dexpgauss(Ydat[i],beta0+beta1 *Xdat[i],sigi,lambda,log = 'true')
    
    
  }
  
  
  
  value1 = min(0,numer+qlamnum-denom-qlamden)
  
  value2 = exp(value1)
  
  value2
  
}
#==================================================================================



# ===============================================================================
#This function getexpecMH simulatees the M-H algorithm for a given sample size and variance of the random walk and initial guesses

#       INPUTS: 
# N             number of MCMC simulations
# varbeta0      variance of proposal of beta0 
# beta0guess    initaial guess for beta01
# varbeta1      variance of proposal of beta1 
# beta1guess    initaial guess for beta01
# lambdaguess   initial guess for lambdaguess

# OUTPUTS: 
# OUtput vector is a 3*N X 1 vector concatenating samples of beta0, beta1 and lambda



getexpecMH = function(N,varbeta0,beta0guess,varbeta1,beta1guess,lambdaguess){
  
  sd0 = sqrt(varbeta0)
  sd1 = sqrt(varbeta1)
  
  beta0Store = 0*1:(N) # Initialization
  beta1Store = 0*1:(N) # Initialization
  lambdaStore = 0*1:(N) # Initialization
  
  beta0Store[1] = beta0guess # Inital Guess
  beta1Store[1] = beta1guess # Inital Guess
  lambdaStore[1] = lambdaguess # Inital Guess
  
  for (i in 2:N){
    
    #------ Update beta0 -----------------------------------------------------------------
    beta0s = rnorm(1,beta0Store[i-1],sd0) # beta1 star (proposal)
    
    U = runif(1,0,1)
    
    if(prob1){
      beta0Store[i] = beta0Store[i-1] # For Problem 1
    }
    
    if(prob2){
      beta0Store[i] = beta0s         # For Problem 2
    }
    
    if(prob3){
      beta0Store[i] = beta0s          # For Problem 3
    }
    
    if(U > alpxy(Ydat,Xdat,beta0Store[i-1],beta0s,beta1Store[i-1],beta1Store[i-1],lambdaStore[i-1],lambdaStore[i-1]) ){
      
      beta0Store[i] = beta0Store[i-1]
      
    }
    
    
    #-----------------------------------------------------------------------------------
    
    #------ Update beta1 -----------------------------------------------------------------
    beta1s = rnorm(1,beta1Store[i-1],sd1) # beta1 star (proposal)
    
    U = runif(1,0,1)
    beta1Store[i] = beta1s
    
    if(U > alpxy(Ydat,Xdat,beta0Store[i],beta0Store[i],beta1Store[i-1],beta1s,lambdaStore[i-1],lambdaStore[i-1]) ){
      
      beta1Store[i] = beta1Store[i-1]
      
    }
    
    
    #-----------------------------------------------------------------------------------
    
    #------ Update lambda -----------------------------------------------------------------
    if(prob1){
      lambdas = lambdaStore[i-1]                                    # Proposal for Problem 1
    }
    
    if(prob2){
      lambdas = rgamma(1,(lambdaStore[i-1])^2,(lambdaStore[i-1])  )  # Proposal for Problem 2 
    }
    
    if(prob3){
      lambdas = rbeta(1,10,10*(1/lambdaStore[i-1] - 1))               # Proposal for Problem 3
    }
    
    U = runif(1,0,1)
    lambdaStore[i] = lambdas
    
    
    if(U > alpxy(Ydat,Xdat,beta0Store[i],beta0Store[i],beta1Store[i],beta1Store[i],lambdaStore[i-1],lambdas) ){
      
      lambdaStore[i] = lambdaStore[i-1]
      
    }
    
    
    #-----------------------------------------------------------------------------------
    
    #cat("\014")
    cat(' \014 Finished ',i/N*100,'% \n')
  }
  
  
  returnVector = c(beta0Store,beta1Store,lambdaStore)
  
  returnVector
  
}

#=============================== MAIN CODE STARTS HERE========================================================================
N = 1000  # numner of iterations


if(prob1){
#---------- These paramerters are for Problem 1. ---------------------

varbeta0 = 0.5  # Any positive number
beta0guess = 5  # Given already

varbeta1 = 1
beta1guess = 0

lambdaguess = .4 # Given already

#-------------------------------------------------------------------
}

if(prob2){
#---------- These paramerters are for Problem 2. ---------------------

varbeta0 = .5
beta0guess = 1

varbeta1 = .5
beta1guess = 1

lambdaguess = 1

#---------------------------------------------------------------------
}

if(prob3){
#---------- These paramerters are for Problem 3. ---------------------


varbeta0 = 0.5
beta0guess = 0

varbeta1 = 1
beta1guess = 0

lambdaguess = .1

# -------------------------------------------------------------------
}

mySamples = getexpecMH(N,varbeta0,beta0guess,varbeta1,beta1guess,lambdaguess)

beta0Samp = mySamples[1:N]
beta1Samp = mySamples[(N+1):(2*N)]
lambdaSamp = mySamples[(2*N+1):(3*N)]


#----------- PLOT DENSITY ----------------------------------------


plot(density(beta0Samp),main =  expression( beta[0] ))
plot(density(beta1Samp),main =  expression( beta[1] ))
plot(density(lambdaSamp),main =  expression( lambda ))

#----------------------------------------------------------------


source("http://www.stat.psu.edu/~mharan/batchmeans.R")

va = bm(beta0Samp)
vb = bm(beta1Samp)
vc = bm(lambdaSamp)

NVector = seq(100,N,10)

muhatVectora = 0*NVector
muhatVectorb = 0*NVector
muhatVectorc = 0*NVector

mcmcVectora = 0*NVector
mcmcVectorb = 0*NVector
mcmcVectorc = 0*NVector

for (i in 1:length(NVector)){
  
  valuesa = bm(beta0Samp[1:NVector[i]])
  valuesb = bm(beta1Samp[1:NVector[i]])
  valuesc = bm(lambdaSamp[1:NVector[i]])
  
  muhatVectora[i] = valuesa[1]
  mcmcVectora[i] = valuesa[2]
  
  muhatVectorb[i] = valuesb[1]
  mcmcVectorb[i] = valuesb[2]
  
  muhatVectorc[i] = valuesc[1]
  mcmcVectorc[i] = valuesc[2]
  
  
  
}

#--------------- PLOT MU HAT AND MCSE vs SAMPLE SIZE ------------------------------------------

par(mfrow=c(2,1))
plot(NVector,muhatVectora,type = 'l',main =  expression('Mean of '* beta[0] ))
plot(NVector,mcmcVectora,type = 'l',main =  expression('MCSE of '* beta[0] ))


par(mfrow=c(2,1))
plot(NVector,muhatVectorb,type = 'l',main =  expression('Mean of '* beta[1] ))
plot(NVector,mcmcVectorb,type = 'l',main =  expression('MCSE of '* beta[1] ))


par(mfrow=c(2,1))
plot(NVector,muhatVectorc,type = 'l',main =  expression('Mean of '* lambda ))
plot(NVector,mcmcVectorc,type = 'l',main =  expression('MCSE of '* lambda ))

#--------------------------------------------------------------------------------------------




#--------------- CALCULATE CONFIDENCE INTERVALS, MUHAT, MCSE HERE---------------------------
v1 = bm(beta0Samp)

conf951 = quantile(beta0Samp,c(0.025, 0.975))

v2 = bm(beta1Samp)

conf952 = quantile(beta1Samp,c(0.025, 0.975))

v3 = bm(lambdaSamp)

conf953 = quantile(lambdaSamp,c(0.025, 0.975))

#-------------------------------------------------------------------------------------------

