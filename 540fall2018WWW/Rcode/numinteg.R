
#########################
## example for integrate
#########################

########################################
# Toy example for numerical integration
# Conditional pdf of mu
########################################

xConst = 0.7
alphaConst = 2
betaConst = 2
  
muCondtl = function(mu)
{
  val1 = exp(-0.5*(xConst-mu)^2)/sqrt(2*pi)
  val2 = (mu^(alphaConst-1)*(1-mu)^(betaConst-1))/beta(alphaConst,betaConst)

  return(val1*val2)
}


muPrior = function(mu)
  {
    val = (mu^(alphaConst-1)*(1-mu)^(betaConst-1))/beta(alphaConst,betaConst)
    return(val)
  }
#muvals = seq(0,1,length=100)
#plot(muvals, sapply(muvals, muCondtl))
#lines(muvals, sapply(muvals, muPrior), col="red")

########################################
## calculate normalizing constant
## for above function
########################################

########################################
## Riemann integration
########################################
approxList = rep(NA,50)
for (i in 1:50)
  {
    n = 10*i # number of intervals
    nodesRie = seq(0,1,length=n)  ## divide up interval into n pieces
    muCondtlEval= sapply(nodesRie, muCondtl)
    approxInt = sum(muCondtlEval)/n
    approxList[i] = approxInt
  }
plot(approxList, ylim=c(0.34,0.4))
abline(h=0.3818935)

## Using R's quadrature function
integrate(muCondtl, 0, 1)
#0.3818935 with absolute error < 4.2e-15
## So approximation to normalizing constant is 1/0.3818935

muCondtlNormalized = function(mu)
  {
    return(muCondtl(mu)/0.3818935)
  }
integrate(muCondtlNormalized,0.5,1)
## 0.5184336 with absolute error < 5.8e-15

###############################################
## Approximating the sampling distribution of 
## skew (asymmetry) and kurtosis ("peakiness")
## skew = third central moment/standard dev^3
## kurtosis = fourth central moment/sd^4
## E(skew) = 0, E(kurtosis)=4
###############################################
M = 1000000 # Monte Carlo sample size
N = 100 # size of N(0,1)

sampleskew=rep(NA, M)
samplekurt=rep(NA, M)
for (i in 1:M)
{
  normvars = rnorm(N, 0,1)
  samplemean=mean(normvars)
  samplesd=sd(normvars)
  sampleskew[i]=mean((normvars-samplemean)^3)/(samplesd^3)
  samplekurt[i]=mean((normvars-samplemean)^4)/(samplesd^4)
}

hist(sampleskew, main=paste("Sample skew for ",N," N(0,1)"))
abline(v=0.6,col="red")
sum(sampleskew>0.6)/M

hist(samplekurt, main=paste("Sample kurtosis for ",N," N(0,1)"))
abline(v=0.5,col="red")
sum(samplekurt>0.5)/M

###############################################
## Approximating an expectation
###############################################


######################################################################
### function to plot estimate of expected value (versus sample size)
### and estimate of MCse (versus sample size)
######################################################################
## Note: can replace below with cumsum function
cummean = function(vec)
{
  cummean = rep(NA, length(vec))
  cummean[1]=vec[1]
  for (i in 2:M)
    cummean[i] = (i-1)/i*cummean[i-1] + vec[i]/i
  return(cummean)
}

cummcse = function(vec)
{
  MINLEN=50
  veclen=length(vec)
  if (veclen<=MINLEN)
    stop("Vector not long enough, should be greater than",MINLEN,"\n")
  cummcse = rep(NA, veclen-MINLEN)
  cummcse[1]=sd(vec[1:MINLEN])/MINLEN
  k=1
  for (i in (MINLEN+1):veclen)
    {
      k = k+1
      cummcse[k] = sd(vec[1:i])/i
    }
  return(cummcse)
}

plotcum=function(vec)
  {
    par(mfrow=c(2,1))
    cumvec = cummean(vec)
    mcsevec = cummcse(vec)
    plot(seq(1,M), cumvec, main="Estimate of expectation versus sample size")
    plot(mcsevec, main="Estimate of standard error versus sample size")
    
    par(mfrow=c(1,1))
  }

M = 1000 # Monte Carlo sample size
######################################################
##### from above, expectation of sample kurtosis
##### for logNormal(0,1) with N=15 samples
######################################################
N=15
samplekurt=rep(NA, M)
for (i in 1:M)
{
  lognormvars = exp(rnorm(N, 0,1))
  samplemean=mean(lognormvars)
  samplesd=sd(lognormvars)
##  sampleskew[i]=mean((lognormvars-samplemean)^3)/(samplesd^3)
  samplekurt[i]=mean((lognormvars-samplemean)^4)/(samplesd^4)
}
plotcum(samplekurt)

#### expectation of a t-5 r.v.
tvars = rt(M, 5)
plotcum(tvars)
## tcum = cummean(tvars)
## tmcse = cummcse(tvars)
## par(mfrow=c(2,1))
## plot(seq(1,M), tcum, main="Estimate of expectation versus sample size")
## plot(tmcse, main="Estimate of standard error versus sample size")

#### expectation of a Cauchy does not exist!!
cauchyvars = rcauchy(M, 0,1)
plotcum(cauchyvars)

