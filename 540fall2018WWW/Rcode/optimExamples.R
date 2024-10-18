##############################
## example of use of optim
## 1D optimization
##############################

## plot g(x)
g = function(x)
  {
    val = exp(-x)*(3.2*sin(x) - 0.5*cos(x))

    return(val)
  }

##xs = seq(0.0001, 10, length=100)
xs = seq(0, 10, length=1000)
ys = sapply(xs, g)
plot(xs, ys, type="l",xlab="x axis")
#abline(v=1)

## but note that optim by default MINIMIZES the function provided
## so best to write negative version of above function

gNegative = function(x)
  {
    val = exp(-x)*(3.2*sin(x) - 0.5*cos(x))

    return(-1*val)
  }

## can now use optim
## used initial value 2
optimval = optim(c(2), gNegative, control=list(trace=TRUE)) # supply initial value = 2 and the function gNegative
abline(v=optimval$par, col="red")
## Result (print out) is below

## > optim(c(2), gNegative)
## $par
## [1] 0.9404297

## $value
## [1] -0.8942613

## $counts
## function gradient 
##       32       NA 

## $convergence
## [1] 0

## $message
## NULL

## e.g. N-R for g(x) = log(x)/(1+x)
g = function(x)
  return(log(x)/(1+x))

g1 = function(x)
  return(-log(x)/((1+x)^2) + 1/(x*(1+x)) )

g2 = function(x)
  return(log(x)/(2*(1+x)^3) - 2/(x*(x+1)^2) - 1/(x*x*(1+x)) )

xs = seq(0.001,3, length=100)
plot(xs, sapply(xs, g), type="l")
plot(xs, sapply(xs, g1), type="l")
plot(xs, sapply(xs, g2), type="l")

### If you want to save the result of the function, which is what you typically want
## do the following
optimRes <- optim(c(2), gNegative)
## optimRes$par provides the parameter value that maximizes the function. If the function were a log-likelihood
## , this would provide the MLE
optimRes$par

##############################
## example: Gamma pdf 1D
##############################
mydat <- scan("mygamma.dat") # true alpha, beta = 7,4
mydatLen <- length(mydat)
loglikeGamma1D <- function(param, xvec=mydat, n = mydatLen)
  {
    ## beta is assumed to be known to be 4
    ## alpha = param
    logval <- -n*log(gamma(param[1])) - n*param[1]*log(4) + (param-1)*sum(log(xvec)) - sum(xvec)/4
    return(logval)
  }

alphavals <- seq(5,9,length=100)
ys <- sapply(alphavals, loglikeGamma1D)
plot(alphavals, ys, type="l")

optim(c(1), loglikeGamma1D, control=list(fnscale=-1))
alphaMOM <- mean(mydat)/4 # when beta = 4
optim(c(1), loglikeGamma1D, control=list(fnscale=-1))

##############################
## example: Gamma pdf 2D
##############################
mydat <- scan("mygamma.dat") # true alpha, beta = 7,4
loglikeGamma <- function(param, xvec=mydat, n = mydatLen)
  {
    ## alpha = param[1], beta = param[2]
    logval <- -n*log(gamma(param[1])) - n*param[1]*log(param[2]) + (param[1]-1)*sum(log(xvec)) - sum(xvec)/param[2]
    return(logval)
  }

optim(c(7,4), loglikeGamma, control=list(fnscale=-1))
betaMOM <- var(mydat)/mean(mydat) 
alphaMOM <- mean(mydat)/betaMOM
optim(c(alphaMOM, betaMOM), loglikeGamma, control=list(fnscale=-1))

