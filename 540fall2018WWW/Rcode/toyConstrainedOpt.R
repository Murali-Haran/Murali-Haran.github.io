## toy example of constrained optimization

set.seed(1)
xs <- rnorm(100, mean=0, sd=1)
n <- length(xs)
sumxssq <- sum(xs^2)

## log likelihood of sigma
loglike <- function(par)
  {
    val <- -n*log(par) - 0.5*sumxssq/(par*par)
    return(val)
  }
sigmavals <- seq(0.2,2,length=100)
ys <- sapply(sigmavals, loglike)
plot(sigmavals, ys, type="l")

## log likelihood of sigmastar = log(sigma)
loglike2 <- function(par)
  {
    sigmastar <- par
    val <- -n*sigmastar - 0.5*sumxssq/(exp(sigmastar)*exp(sigmastar))
    return(val)
  }

sigmastarvals <- seq(log(0.2),log(2),length=100)
ysstar <- sapply(sigmastarvals, loglike2)
plot(sigmastarvals, ysstar, type="l",xlab="log(sigma)")

####
test <- function(x)
  return(x*x + (x-1)*(x-1)*(x-1))
