## In homework 6, you will have to simulate a number of
## random variables with density
## f(x) = cx^(a-1)  for 0<x<d,
## where c, a, and d are positive constants.  (Obviously, 
## the value of c is uniquely determined by a and d.)
## 
## Such random variables are beta(a, 1) variables multiplied
## by d.  This is because the beta density with parameters (a, b)
## is proportional to x^(a-1)(1-x)^(b-1) for 0<x<1.
## Here is an example:

## Generate 30 random variables with density cx^2 for 0<x<2:
X <- rbeta(30, 3, 1) * 2


