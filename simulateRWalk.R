######################################
## R code to simulate a random walk ##
## in 1D                            ##
######################################

N=100 # length of random walk
pMoveRight = 0.5 # probability of moving to the right

X = rep(NA, N) # vector of length N, filled with NA

X[1]=0 # start the random walk at 0
for (i in 2:N)
  {
    genUnif = runif(1) # generate a single Unif(0,1) r.v.
    if (genUnif<pMoveRight)
      X[i] = X[i-1] + 1 # move right w/ prob pMoveRight
    else
      X[i] = X[i-1] - 1 # move left w/ prob (1-pMoveRight)
  }

######################################
## plot the random walk sample path ##
######################################

xVals = seq(1,N)
yVals = X
plot(xVals, yVals, pch=19, col="red") # plot random walk with solid circles (pch=19)

######################################
## calculations based on sample path##
######################################

sum(X) # total
mean(X)
sd(X)
