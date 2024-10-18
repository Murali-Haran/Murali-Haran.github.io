################################
## If else
################################

if( any(x <= 0) ) {
  x = 1+x
}
y = log(x)

################################
## Assigning result of if/else
################################

if ( any(x <= 0) ) { 
  y = log(1+x) 
} else {
  y = log(x)
}

## Above is the same as below
y = if ( any(x <= 0) ) {
  log(1+x)
} else {
  log(x)
}

################################
## Example of above but to check
## validity of argument
################################
myMedian = function(x, hi = NULL){
  if (is.null(hi)) {
    median(x)
  } else {
    median(x[order][-1])
}

################################
## Checking for errors
## Handling errors
################################
ratio = 
  if (x != 0) {
    y/x 
  } else {
    NA
  }

### OR
  ratio = if (x !=0) y/x else NA

################################
### Function to debug
################################

  whisker.endpoints = function(x) {
  if (is.na(x) warning("Careful x has NAs")
  qlu = quantile(x, probs = c(0.25, 0.75), 
                     na.rm = FALSE)
  iqr = IQR(x, na.rm = FALSE)
##  return(x - 1.5 * iqr, x + 1.5 * iqr)
      return(list(x - 1.5 * iqr, x + 1.5 * iqr))
}

################################
###  Hard to read code
################################
disobey = function(x,epsilon){ 
if(any(x<=0)){x[x<=0]=epsilon} 
for(i in x){g(i)}}

### better than above
obey = function(x, epsilon) { 
  if(any(x <= 0)) {
    x[x <= 0] = epsilon 
  }
  for (i in x) {
    cat(i,"\n")
  }
}

################################
###  Catching errors
################################
  
  showstop = function(x){
      if(any(x < 0)) stop("x must be >= 0")
      return("ok")
  }
  
showstop(1:5)
##[1] "ok"

showstop(c(-1, 1))
  ##Error

################################
### Showstopifnot function
### More options
################################
  
showstopifnot = function(x){
  stopifnot(x >= 0, x %% 2 == 1)
  return("ok")
}

showstopifnot(1)
##[1] "ok"

showstopifnot(c(1, -1))
##Error: all(x >= 0) is not TRUE

showstopifnot(c(1,2))
##Error: x %%2 == 1 is not all TRUE

### print warning messages
ratio.warn = function(x, y){
  if(any(y == 0))
    warning("Dividing by zero")
  return(x/y)
}

ratio.warn(x = 1, y = c(1, 0))
###[1]   1 Inf
###Warning message:
###In ratio.warn(x = 1, y = c(1, 0)) : Dividing by zero

  ratio.warn(x = 1:3, y = 1:2)

### try allows for code to be tried out
### if no error, proceed. If error, decide what to do
ratio.try = function(x, y){
  z = try(x/y, silent = TRUE) 
  if(inherits(z, "try-error")) {
     warning("Division problem")
     z = NULL
  }
  return(z)
}

ratio.try(x = 1, y = c(1, 0))
###[1]   1 Inf

ratio.try(x = 1, y = "r")
###NULL
###Warning: Division problem

#### Checking how much time it takes code to run
system.time((normal.samples = rnorm(1000000)))
#### Systematically: use Rprof and summaryRprof


################################
### Loops
################################

doubleBet = function(n) {
   
  urn = c(-1, 1)
  
  for (i in 1:n) {
    res = sample(urn, size = 1)
    if (res > 0) return(i)
  }
  return(NA)
}

################################
### Better version with checks
################################

  doubleBet = function(n) {
  
  if(!is.numeric(n)) stop("n must be numeric")
 
  urn = c(-1, 1)
  
  for (i in 1:n) {
    res = sample(urn, size = 1)
    if (res > 0) return(i)
  }
  return(NA)
}

################################
### While loops
################################

doubleWhile = function(){

 bets = 0
 urn = c(-1, 1)
 res = -1

 while (res < 0) {
  res = sample(urn, 1)
  bets = bets + 1
 }
 return(bets)
}

###numBetsUntil = replicate(100000, double.Inf())
  numBetsUntil = replicate(100000, doubleWhile())

summary(numBetsUntil)

walletSize = 2^numBetsUntil – 1

### Using break statement
  
doubleWhile = function(){

 res = -1
 bets = 0
 max.iter = 1000
 urn = c(-1, 1)

 while(res < 0){
  res = sample(urn, 1)
  bets = bets + 1
  if(bets > max.iter){
    warning(“Maximum iteration reached”)
    break
  }
 }
 return(bets)
}

### Vector version
  
double.vec = function(n) {
  
  res = sample(c(-1, 1), size = n, 
               replace = TRUE)
 
  firstWin = which(res > 0)[1]
  
  if (length(firstWin) == 0) return(NA)  
  return(firstWin)
}

#### Comparing costs
system.time(replicate(100000, doubleBet(200)))
  ##  user  system elapsed 
  ## 1.738   0.167   1.952 
 
system.time(replicate(100000, doubleBet.vec(200)))
## user  system elapsed 
##   1.906   0.138   2.063 
