## Murali Haran wrote the original version of this document.  Thanks, Murali!

## To generate values from a uniform density, use the command 'runif'.
## To find out how to use R commands, use `help', for example: 
help(runif)  # if you are within a unix/command line view, you can hit "q" to quit help
## generate 1000 uniform(4,9) r.v.s, store in mysim, a vector of 100 values.
mysim <- runif(1000, 4, 9) 

## Similarly, you can generate 5000 random variates from a Poisson(25) distribution
mysim <- rpois(5000, 25)

## To compute the mean and variance of an R vector, say mysim, you would use the commands: 
mean(mysim) 
var(mysim)

## You can initialize a vector in R using `rep'
foo <- rep(NA, 1000) # this creates a vector of length 100 with all NAs (missing values)
foo <- rep(0, 1000) # this creates a vector of length 100 with all 0s

## A programming loop (to repeat some code) can be written in R using a "for loop". For example:
NUMREP <- 1000
foo <- rep(0, NUMREP)
for (i in 1:NUMREP) # repeat code below NUMREP times
  {
    mysim <- runif(50) #  generate 50 random unif(0,1)
    foo[i] <- sum(mysim<0.3) # count number of values in the vector mysim that are less than 0.3
  }

myestimate <- mean(foo) 
myestimate

## You can store the commands you want to run in a file, say myfile.R.
## To run your commands, simply type in:
source("myfile.R") # making sure that myfile.R is in the same directory that you are running R from

## to simulate from a multi-level random process (a process that involves two levels of randomness)
## (similar to toy example from class)
## e.g. Y ~ Poisson(20), X|Y ~ Binomial(Y, p=0.7)
ys <- rpois(1000, 20)
xs <- rep(NA, length(ys))

for (i in 1:length(ys))
  xs[i] <- rbinom(1, ys[i], 0.7)

## a more elegant and potentially faster way to do the above
## is to avoid loops and use the "apply" function.  (R loops can be slow.)
myfunction <- function(x) return(rbinom(1, x, 0.7)) # this function takes in a value x, and returns a Binomial(x,0.7) r.v.
xs <- sapply(ys, myfunction)  # apply myfunction to each element in the vector ys

