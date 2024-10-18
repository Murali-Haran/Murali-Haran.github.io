#############################################
## bisection algorithm
## maximizing log(x)/(1+x)
#############################################

## plot g(x)
g = function(x)
  {
    return(log(x)/(1+x))
  }

xs = seq(0.000,5, length=100)
ys = sapply(xs, g)

plot(xs,ys, type="l")

## plot g'(x)
gprime = function(x)
  {
##    val = (-log(x)*(1+x) + x)/ ((1+x)*(1+x)*x)
    val = -log(x)/((1+x)*(1+x))  + 1/(x*(1+x))
##    val = (1 + 1/x - log(x))/((1+x)*(1+x))
      
    return(val)
  }

##xs = seq(0.000,5, length=100)
#xs = seq(2,4, length=100)
ys = sapply(xs, gprime)

plot(xs,ys, type="l")
locator(1)

## bisection algorithm

##initval = 3 # initial guess based on plot

currentA = 1 # initial lower end point
currentB = 5 # initial upper end point
currentRoot = (currentA+currentB)/2 # initial guess of root

estimateRoot = rep(NA, 11)
estimateA = rep(NA, 11)
estimateB = rep(NA, 11)


estimateRoot[1] = currentRoot
estimateA[1] = currentA
estimateB[1] = currentB

#plot(seq(-1,10), seq(-1,10), type="n")

## repeatedly update the "guess"
for (i in 1:10)
  {
    
    ##  if gprime has same signs at A,X
    if ((gprime(currentA)*gprime(currentRoot)) > 0)
      {
        currentA = currentRoot # move lower end point to best guess
        currentB = currentB # keep upper end point as is
      }
    else  ##  if gprime has different signs at A,X
      {
        currentA = currentA # keep lower end point as is
        currentB = currentRoot # move upper end point to best guess
      }
    currentRoot = (currentA+currentB)/2 # new guess of root

    estimateRoot[i+1] = currentRoot
    estimateA[i+1] = currentA
    estimateB[i+1] = currentB

    segments(estimateA[i+1], 2, estimateB[i+1], 2, col="blue")

  }

## plot how the estimate improves with iterations
plot(seq(1,11),estimateRoot)

cbind(estimateA, estimateB, estimateRoot)

#############################################
## Newton-Raphson
## maximizing log(x)/(1+x)
#############################################

## plot g(x)
g = function(x)
  {
    return(log(x)/(1+x))
  }

xs = seq(0.000,5, length=100)
ys = sapply(xs, g)

plot(xs,ys, type="l")

## plot g'(x)
gprime = function(x)
  {
##    val = (-log(x)*(1+x) + x)/ ((1+x)*(1+x)*x)
    val = -log(x)/((1+x)*(1+x))  + 1/(x*(1+x))
##    val = (1 + 1/x - log(x))/((1+x)*(1+x))
      
    return(val)
  }


## plot g''(x)
gprimeprime = function(x)
  {
    val = log(x)/(2*(1+x)*(1+x)*(1+x))  -2/(x*(1+x)*(1+x)) -1/(x*x*(1+x))
      
    return(val)
  }

##xs = seq(0.000,5, length=100)
#xs = seq(2,4, length=100)
ys = sapply(xs, gprime)

plot(xs,ys, type="l")
locator(1)

## Newton-Raphson algorithm

##initval = 3 # initial guess based on plot

estimateRoot = rep(NA, 11)

estimateRoot[1] = 3
  
## repeatedly update the "guess"
for (i in 1:10)
  {
    estimateRoot[i+1] = estimateRoot[i] - gprime(estimateRoot[i])/gprimeprime(estimateRoot[i])
  }

## plot how the estimate improves with iterations
plot(seq(1,11),estimateRoot)
