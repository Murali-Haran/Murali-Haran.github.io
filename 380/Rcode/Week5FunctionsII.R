x = c(1, 0, 0, 5, 3)
## To find the median, we sort the values:
sort(x)

## Select the middle one:
sort(x)[3]

x = c(1, 0, 0, 5, 3)

## encapsulate into a function
myMedian = function(x) {
  sort(x)[3]
}

## We need to make it work for vectors of other lengths

myMedian = function(x) {
  sort(x)[length(x)/2]
}

x = c(1, 0, 0, 5, 3)

myMedian(x)

##

myMedian = function(x) {
  sort(x)[(length(x) + 1)/2]
}


## OR: A bit easier to read if we create a variable for length(x)

myMedian = function(x) {
  n = length(x)
  sort(x)[(n + 1)/2]
}

## Testing code: 
x = c(1, 0, 0, 5, 3)
myMedian(x)

## Another test

y = c(1, 0, 1, 5, 3, 20)
myMedian(y)

## Control flow example: 
## ODD: We want to choose the (n+1)/2 largest element when n is odd
## sort(x)[(n + 1)/2]
## EVEN: We want to average the n/2 and n/2 + 1 largest elements when even
## mean(sort(x)[c(n/2, n/2 + 1)])

myMedian = function(x) {
  n = length(x)
  odd = as.logical(n %% 2)

  if (odd) {
    sort(x)[(n + 1)/2]
  } else {
    mean(sort(x)[c(n/2, n/2 + 1)])
  }
}

## Another version 
myMedian = function(x) {
  n = length(x)
  odd = as.logical(n %% 2)

  if (odd) {
    m = sort(x)[(n + 1)/2]
  } else {
    m = mean(sort(x)[c(n/2, n/2 + 1)])
  }
  return(m)
}
## Alternative 
myMedian = function(x) {
  n = length(x)
  odd = as.logical(n %% 2)

  if (odd) {
    return(sort(x)[(n + 1)/2])
  } else {
   return(mean(sort(x)[c(n/2, n/2 + 1)]))
  }
}


## Version that allows for three different kinds of medians
myMedian = function(x, hi = NULL) {
  n = length(x)
  odd = as.logical(n %% 2)

  if (odd) {
    return(sort(x)[(n + 1)/2])
  } else if(is.null(hi)) { 
    return(mean(sort(x)[c(n/2, n/2 + 1)]))
    }  else if(hi) {
       return(sort(x)[n/2 + 1])
       } else return(sort(x)[n/2])  
}

###################################
## Use the ... argument
## to allow for extra arguments
## to the function provided
###################################

squaredFn=function(x, func=mean,...)
{
  sqvals=x^2
  func(sqvals, ...)
}
### examples:
squaredFn(rnorm(10))
squaredFn(rnorm(10), func=quantile, probs=c(0.02))

###################################
### if/else statements
###################################
## The result of an if/else statement can be assigned.  For example,

if ( any(x <= 0) ) { 
  y = log(1+x) 
} else {
  y = log(x)
}

## is the same as
y = if ( any(x <= 0) ) {
  log(1+x)
} else {
  log(x)
}

## Also, the else clause is optional.   Another way to do the above is

if( any(x <= 0) ) {
  x = 1+x
}
y = log(x)

## 1. With logical arguments to tell a function what to do

myMedian = function(x, hi = NULL)
{
    if (is.null(hi)) {
        median(x)
    } else {
        median(x[order][-1])
    }
}

## 2. To verify that the arguments of a function are as expected
  
if ( !is.matrix(m) ) {
    stop("m must be a matrix")
}
  
## To handle common numerical errors
ratio = 
  if (x != 0) {
    y/x 
  } else {
    NA
  }

## This can be more compactly written because each block consists of only one statement
ratio = if (x !=0) y/x else NA
  

