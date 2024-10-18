#Matrix definition example
# byrow is set to FALSE by default which means it reads in data by columns (as in FORTRAN)
# if byrow is set to TRUE, it reads in data by row
foo <- matrix(c(3,5,2,46,2,4,728,34,4),byrow=TRUE,3,3)

## The right way to read in the data if you are interested in storing it into a matrix {y_ij}
> y <- scan("foodata")  # read in the data
> y <- matrix(y,13,3,byrow=TRUE)  # convert the data into the appropriate matrix (say 13 x 3)

## simulate from multivariate normal densities ##
## If you want to simulate from a multivariate normal density, you have a couple of different options :
#(1) use the library MASS which has a built in mvrnorm (multivariate normal density simulator). The help function provides nice examples of how to use it.
> library(MASS)
> help(mvrnorm)

#(2) If you are unable to find the library MASS or are using a
#programming language other than R, you would simulate from a
#multivariate normal by simulating a vector of iid normals and then
#multiplying it by a choleski decomposition of the covariance matrix.

#If you want to simulate an m-dimensional vector ~ N(mu, Sigma) where
#mu is a vector and Sigma is a variance-covariance matrix, do the
#following:
#(1) Simulate a vector of m i.i.d. N(0,1), call it z.
#(2) Find the choleski decomposition of Sigma. You would use the function "chol" in R.
> Sigmachol <- chol(Sigma)
#(3) Multiply z by Sigmachol.  In R: 
> t(Sigmachol)%*%z  # the t(Sigmachol) takes the transpose of Sigmachol (why you need the transpose follows from basic multivariate normal theory)
#(4) Add the mean vector. In R:
> multsamp <- t(Sigmachol)%*%z + mu

#multsamp is a random draw from a N(mu,Sigma) 

## 
#Creating figures and saving them into jpeg format:
xs =seq(-5,5,by=0.1) # list of values to plot
sqfun=function(x) 
 return(x^2)

## using the 'sapply' function (fast)
ys=sapply(xs,sqfun)

## this will allow you to look at it
plot(xs,ys)

## to save the results in jpeg format
jpeg("mypic.jpg")
plot(xs,ys)
dev.off()  ## kill jpeg device

## Now you can include mypic.jpg in your LaTeX file or anywhere else

## sapply with an indicator function:
## one way to count the number of xs> 1
xs <- rnorm(10000)
sum(xs>1)

## using indicator functions (if you actually need the individual indicator values which are TRUE or FALSE)
indfun <- function(x) 
  return(x>1)

indvals <- sapply(xs,indfun)
## of course this command would produce the same result as sum(xs>1)
sum(indvals)

###########################################################################################################
##  A useful tool for profiling your code (determining which parts of your program are taking the most time
## It breaks down total run time by functional call (including O
## time. custom functions). This can let you find bottlenecks (and errors), which can dramatically 
## reduce computing time.
## (thanks to C.Congdon for the pointer)
###########################################################################################################
Rprof() #Begin Tracking Function Time
res <- test2() #Run Function (what you want to clock)
Rprof(NULL) #Stop Tracking Function Time
summaryRprof() #Print Summary

