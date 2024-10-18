########################
## simulating from a
## two-stage (hierarchical)
## model
########################

## method 1
M=10000 # number of replicates
Y = rpois(1, 14) # number of eggs laid, Y ~ Pois(14)
X = rbinom(M, Y, 0.3) # number of surviving eggs, X| Y ~ Bin(Y, p=0.3)

## method 2
M=10000 # number of replicates
Y = rpois(M, 14) # number of eggs laid, Y ~ Pois(14)
X = rep(NA, M) # number of surviving eggs, X| Y ~ Bin(Y, p=0.3)
for (i in 1:M)
  X[i] = rbinom(1,Y[i],0.3)


### simple mixture model
## Y ~ Ber(0.7)
## X | Y=0 ~ N(10,1)
## X | Y=1 ~ N(20,3)

M=10000 # number of replicates
Y = rbinom(M, 1,0.7) 
X = rep(NA, M)
for (i in 1:M)
  {
    if (Y[i]==0)
      X[i] = rnorm(1,10,1)
    else
      X[i] = rnorm(1,20,3)
  }
