########################################################################
# HOW TO RUN: just highlight everything and run it. Everything is more #
#             or less in the order in which it is discussed in my      #
#             writeup.                                                 #
########################################################################

source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R')
source("http://www.stat.psu.edu/~mharan/batchmeans.R")
require("MASS")

#################################################################################
#                                                                               #
#                                  PROBLEM 1                                    #
#                                                                               #
#################################################################################


set.seed(515)

data = read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat')
x = data[,1]
y = data[,2]
### plot the likelihood function
likelihood <- function(b) {
  return(sum(dexpgauss(y, mu=5+b*x, sigma=1, lambda=0.4, log=T)))
}
vals = seq(6,9,by=.001)
liks = rep(NA, length(vals))
for(i in 1:length(vals)) {
  liks[i] = likelihood(vals[i])
}
plot(vals, liks, type='l', xlab="b1", ylab="log-likelihood")
### Looks like MLE is around 7.5 for this data; good starting point
########## DENSITIES ##########
### prior on b1 is N(0,10)
prior <- function(b) {
  return(dnorm(b, mean=0, sd=10, log=T))
}
### log posterior is log likelihood plus log prior
posterior <- function(b) {
  return(likelihood(b)+prior(b))
}
### proposal density is normal (random walk M-H)
proposal <- function(b, var) {
  return(rnorm(1, mean=b, sd=sqrt(var)))
}
########## MAIN ALGORITHM ##########
mh <- function(init, iter, var, print=T) {
  cat("Starting Metropolis-Hastings algorithm...\n")
  cat("Initial value =", init, ",", iter, "iterations\n")
  ptm <- proc.time()
  acc.cnt = 0
  chain = rep(NA, iter+1)
  chain[1] = init
  # main loop
  for(i in 1:iter) {
    # generate proposal
    prop = proposal(chain[i], var)
    # alpha for Metropolis update is ratio of posteriors
    prob = exp(posterior(prop) - posterior(chain[i]))
    if(runif(1) < prob) {
      # accept proposal
      chain[i+1] = prop
      acc.cnt = acc.cnt + 1
    } else {
      # reject proposal; next state = current state
      chain[i+1] = chain[i]
    }
  }
  cat("...done.\n\n")
  # print helpful information
  if(print) {
    est = bm(chain)
    cat("mean =", est[[1]], "\n")
    cat("se =", est[[2]], "\n")
    cat("95% credible interval = (", quantile(chain, c(.025,.975)), ")\n")
    accept = acc.cnt/iter
    cat("Accept rate:", accept, ". Runtime (seconds):\n")
    print(proc.time() - ptm)
  }
  return(chain)
}
########## DIAGNOSTICS ##########
### run algorithm at various start points
chain.init1 = mh(5, 100000, 1)
chain.init2 = mh(7.5, 100000, 1)
chain.init3 = mh(10, 100000, 1)
### plot density for one
par(mfrow=c(1,1))
plot(density(chain.init2), main="")
### plot estimates vs sample sizes for all 3 runs
plot(seq(1, 100000, length.out=10), seq(7.3, 7.36, length.out=10), type='n', xlab="n", ylab="b1 estimate")
p.1 = estvssamp(chain.init1, retval=T)
p.2 = estvssamp(chain.init2, retval=T)
p.3 = estvssamp(chain.init3, retval=T)
points(p.1, type='l', lwd=2)
points(p.2, type='l', lwd=2, col='blue')
points(p.3, type='l', lwd=2, col='red')
### autocorrelation for one
acf(chain.init2, main="")
### effective sample size for one
ess(chain.init2)


#################################################################################
#                                                                               #
#                                  PROBLEM 2                                    #
#                                                                               #
#################################################################################


set.seed(515)
### define erfc function
erfc <- function(x) 2*pnorm(-sqrt(2)*x)
### define score function; used in finding the MLE
score <- function(params) {
  b0 = params[1]; b1 = params[2]; lambda = params[3]
  p.1 <- n*lambda - sum(sqrt(2/pi)*exp(-(b0+b1*x+lambda-y)^2/2)/erfc((b0+b1*x+lambda-y)/sqrt(2)))
  p.2 <- lambda*sum(x) - sum(sqrt(2/pi)*x*exp(-(b0+b1*x+lambda-y)^2/2)/erfc((b0+b1*x+lambda-y)/sqrt(2)))
  p.3 <- n/lambda + n*b0 + b1*sum(x) + lambda*n - sum(y) - sum(sqrt(2/pi)*exp(-(b0+b1*x+lambda-y)^2/2)/erfc((b0+b1*x+lambda-y)/sqrt(2)))
  return(c(p.1,p.2,p.3))
}
########## DENSITIES ##########
##### THESE ARE ALL LOG SCALE
### Helper function that deals with erfc
erfc.part <- function(b0, b1, lambda, x, y) {
  return(mean(log(erfc((b0+b1*x+lambda-y)/sqrt(2)))))
}
### full conditional for b0
fullcond.b0 <- function(b0, b1, lambda, x, y, n) {
  return(n*(lambda*b0 + erfc.part(b0, b1, lambda, x, y)) - b0^2/200)
}
### full conditional for b1
fullcond.b1 <- function(b0, b1, lambda, x, y, n) {
  return(n*(lambda*b1*mean(x) + erfc.part(b0, b1, lambda, x, y)) - b1^2/200)
}
### full conditional for lambda
fullcond.lambda <- function(b0, b1, lambda, x, y, n) {
  return(n*(log(lambda) + lambda*b0 + lambda*b1*mean(x) + lambda^2/2 - lambda*mean(y) + erfc.part(b0, b1, lambda, x, y)) - 0.99*log(lambda) - lambda/100)
}
########## METROPOLIS ALGORITHM ##########
mh <- function(X, Y, N, init, print=T) {
  ### NOTE: the following commented out code was used to calculate the MLE
  ###       and the Hessian using Newton-Raphson via the "rootSolve" package
  ###       I commented it out so that the package didn't need to be loaded.
  ###       The results are hard-coded instead.
  
  #   cat("Finding MLE and Hessian for tailored proposal distribution...\n")
  #   ss <- multiroot(f = score, start = c(0, 1, 0.5))
  #   root <- ss$root
  #   hess <- gradient(f = score, x = root)
  #   sigma <- solve(-hess)
  #   sigma[lower.tri(sigma)] = t(sigma)[lower.tri(sigma)]
  #   cat("...done. MLE is", root, ".\n\n")
  root <- c(2.3498083, 3.4605591, 0.8033067)
  sigma <- matrix(c(0.01851970, -0.0222845462, 0.0035374198,
                    -0.0222845462, 0.0441663004, -0.0002634605,
                    0.0035374198, -0.0002634605, 0.0034890968),
                  nrow=3, ncol=3)
##### MLE RESULTS FOR SIMULATION STUDY. COMMENT ABOVE ROOT AND SIGMA,
##### UNCOMMENT THE ROOT AND SIGMA BELOW, THEN UNCOMMENT THE LINES
##### AT THE END OF PROBLEM 2 TO RUN SIMULATION STUDY.
#   root <- c(4.0734473, 1.6270028, 0.7404077)
#   sigma <- matrix(c(0.020587504, -0.0258746537, 0.0028809126,
#                     -0.0258746537, 0.0506625395, 0.001260909,
#                     0.0028809126, 0.001260909, 0.0027113508),
#                   nrow=3, ncol=3)
  
  cat("Starting Metropolis-Hastings algorithm...\n")
  cat("Initial value =", init, ",", N, "iterations\n")
  ptm <- proc.time()
  
  n = length(X)
  acc.b0 = 0; acc.b1 = 0; acc.l = 0
  
  chain = matrix(NA, N, 3)
  chain[1,] = init
  ### main loop
  for(i in 2:N) {
    curr <- chain[i-1,]
    names(curr) <- c('b0','b1','lambda')
    # generate proposals from multivariate distribution
    prop <- mvrnorm(n=1, mu=root, Sigma=sigma)
    names(prop) <- c('b0','b1','lambda')
    
    ## M-H accept-reject, b0
    logMH = fullcond.b0(prop['b0'], curr['b1'], curr['lambda'], X, Y, n) - fullcond.b0(curr['b0'], curr['b1'], curr['lambda'], X, Y, n)
    logalpha.0 = min(0, logMH)
    if(log(runif(1)) < logalpha.0) {
      curr['b0'] = prop['b0']
      acc.b0 = acc.b0 + 1
    }
    
    ## M-H accept-reject, b1
    logMH = fullcond.b1(curr['b0'], prop['b1'], curr['lambda'], X, Y, n) - fullcond.b1(curr['b0'], curr['b1'], curr['lambda'], X, Y, n)
    logalpha.1 = min(0, logMH)
    if(log(runif(1)) < logalpha.1) {
      curr['b1'] = prop['b1']
      acc.b1 = acc.b1 + 1
    }
    
    ## M-H accept-reject, lambda
    if(prop['lambda'] >= 0) {
      logMH = fullcond.lambda(curr['b0'], curr['b1'], prop['lambda'], X, Y, n) - fullcond.lambda(curr['b0'], curr['b1'], curr['lambda'], X, Y, n)
      logalpha.l = min(0, logMH)
      if(log(runif(1)) < logalpha.l) {
        curr['lambda'] = prop['lambda']
        acc.l = acc.l + 1
      }
    }
    
    ## Update
    chain[i,] = c(curr['b0'], curr['b1'], curr['lambda'])
  }
  time <- proc.time() - ptm
  cat("...done.\n\n")
  ### some helpful results
  if(print) {
    est.b0 = bm(chain[,1])
    est.b1 = bm(chain[,2])
    est.lambda = bm(chain[,3])
    cat("b0:\n\tmean =", est.b0[[1]], "\n\tse =", est.b0[[2]], "\n\taccept rate =", acc.b0/N, "\n\t95% credible interval = (", quantile(chain[,1], c(.025, .975)), ")\n")
    cat("b1:\n\tmean =", est.b1[[1]], "\n\tse =", est.b1[[2]], "\n\taccept rate =", acc.b1/N, "\n\t95% credible interval = (", quantile(chain[,2], c(.025, .975)), ")\n")
    cat("lambda:\n\tmean =", est.lambda[[1]], "\n\tse =", est.lambda[[2]], "\n\taccept rate =", acc.l/N, "\n\t95% credible interval = (", quantile(chain[,3], c(.025, .975)), ")\n")
  }
  cat("Run time:\n")
  print(time)
  return(chain)
}
### load data for problem 2
data = read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat')
x = data[,1]
y = data[,2]
### run algorithm
init = c(2.5, 3.5, 0.8)
chain = mh(x, y, 50000, init)
### plot densities
par(mfrow=c(1,3))
plot(density(chain[,1], adjust=1), main="b0")
plot(density(chain[,2], adjust=1), main="b1")
plot(density(chain[,3], adjust=1), main="lambda")
### make sure estimates are settling down
par(mfrow=c(1,3))
estvssamp(chain[,1])
estvssamp(chain[,2])
estvssamp(chain[,3])
par(mfrow=c(1,1))
### find correlation between b0 and b1
cor(chain[,1], chain[,2])
### check out autocorrelations
acf(chain[,1])
acf(chain[,2])
acf(chain[,3])
par(mfrow=c(1,1))
### find ess for each parameter
ess(chain[,1])
ess(chain[,2])
ess(chain[,3])


##### UNCOMMENT TO RUN SIMULATION STUDY. MUST ALSO UNCOMMENT
##### BLOCK OF CODE IN MH FUNCTION ABOVE
# b0 = 4; b1 = 1.5; lambda = 0.7; n = 500
# x = runif(n); y = rexpgauss(n, mu=b0+b1*x, sigma=1, lambda=lambda)
# init = c(0, 1, 0.5)
# chain = mh(x, y, 50000, init)




#################################################################################
#                                                                               #
#                                  PROBLEM 3                                    #
#                                                                               #
#################################################################################
### NOTE: For the results in my writeup, I used a sample size of 1,000,000 in
###       order to get decent effective sample sizes. This takes about 15
###       to run, however, so here I set 100,000 iterations so that my code
###       as a whole runs in a reasonable amount of time. Changing N to
###       1,000,000 should reproduce the results in my writeup.

set.seed(515515)

mh3 <- function(X, Y, N, init, print=T) {  
  cat("Starting Metropolis-Hastings algorithm...\n")
  cat("Initial value =", init, ",", N, "iterations\n")
  ptm <- proc.time()
  
  n = length(X)
  acc.b0 = 0; acc.b1 = 0; acc.l = 0
  
  chain = matrix(NA, N, 3)
  chain[1,] = init
  
  for(i in 2:N) {
    curr <- chain[i-1,]
    names(curr) <- c('b0','b1','lambda')
    # random walk proposal, from multivariate normal distribution
    prop <- mvrnorm(n=1, mu=curr, Sigma=5*diag(3))
    names(prop) <- c('b0','b1','lambda')
    
    ## M-H accept-reject, b0
    logMH = fullcond.b0(prop['b0'], curr['b1'], curr['lambda'], X, Y, n) - fullcond.b0(curr['b0'], curr['b1'], curr['lambda'], X, Y, n)
    logalpha.0 = min(0, logMH)
    if(log(runif(1)) < logalpha.0) {
      curr['b0'] = prop['b0']
      acc.b0 = acc.b0 + 1
    }
    
    ## M-H accept-reject, b1
    logMH = fullcond.b1(curr['b0'], prop['b1'], curr['lambda'], X, Y, n) - fullcond.b1(curr['b0'], curr['b1'], curr['lambda'], X, Y, n)
    logalpha.1 = min(0, logMH)
    if(log(runif(1)) < logalpha.1) {
      curr['b1'] = prop['b1']
      acc.b1 = acc.b1 + 1
    }
    
    ## M-H accept-reject, lambda
    if(prop['lambda'] >= 0) {
      logMH = fullcond.lambda(curr['b0'], curr['b1'], prop['lambda'], X, Y, n) - fullcond.lambda(curr['b0'], curr['b1'], curr['lambda'], X, Y, n)
      logalpha.l = min(0, logMH)
      if(log(runif(1)) < logalpha.l) {
        curr['lambda'] = prop['lambda']
        acc.l = acc.l + 1
      }
    }
    
    ## Update
    chain[i,] = c(curr['b0'], curr['b1'], curr['lambda'])
  }
  time <- proc.time() - ptm
  cat("...done.\n\n")
  # helpful info
  if(print) {
    est.b0 = bm(chain[,1])
    est.b1 = bm(chain[,2])
    est.lambda = bm(chain[,3])
    cat("b0:\n\tmean =", est.b0[[1]], "\n\tse =", est.b0[[2]], "\n\taccept rate =", acc.b0/N, "\n\t95% credible interval = (", quantile(chain[,1], c(.025, .975)), ")\n")
    cat("b1:\n\tmean =", est.b1[[1]], "\n\tse =", est.b1[[2]], "\n\taccept rate =", acc.b1/N, "\n\t95% credible interval = (", quantile(chain[,2], c(.025, .975)), ")\n")
    cat("lambda:\n\tmean =", est.lambda[[1]], "\n\tse =", est.lambda[[2]], "\n\taccept rate =", acc.l/N, "\n\t95% credible interval = (", quantile(chain[,3], c(.025, .975)), ")\n")
  }
  cat("Run time:\n")
  print(time)
  return(chain)
}

data = read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat')
x = data[,1]
y = data[,2]

init = c(0, 2, 0.16)
### run algorithm
chain.3 = mh3(x, y, 100000, init)
### plot densities
par(mfrow=c(1,3))
plot(density(chain.3[,1]), main='')
plot(density(chain.3[,2]), main='')
plot(density(chain.3[,3]), main='')
### look at autocorrelation plots
acf(chain.3[,1])
acf(chain.3[,2])
acf(chain.3[,3])
### ensure that estimates are settling
par(mfrow=c(1,3))
estvssamp(chain.3[,1])
estvssamp(chain.3[,2])
estvssamp(chain.3[,3])
par(mfrow=c(1,1))
### check ESS
ess(chain.3[,1])
ess(chain.3[,2])
ess(chain.3[,3])