######################################
## Social mobility example
## states = {upper, middle, lower}
## (coded as {3,2,1})
######################################
N = 10000 # length of chain
mchain = rep(NA, N )
mchain[1] = 2 # inital state = middle class

#### generate the remainder of the Markov chain
#### using the given transition prob matrix
for (i in 2:N)
  {
    U = runif(1) # generate U(0,1)
    
    if (mchain[i-1]==3) # upper class
      {
        if (U<0.45)
          mchain[i]=3 # stay
        else
          if (U<0.93)
            mchain[i]=2 # move to middle
          else
            mchain[i]=1 # move to lower
      }
    else
        if (mchain[i-1]==2)  # middle class
          {
            if (U<0.05)
              mchain[i]=3 # move to upper
            else
              if (U<0.75) 
                mchain[i]=2 # stay
              else
                mchain[i]=1 # move to lower
          }
        else # lower class (1)
          {
            if (U<0.01)
              mchain[i]=3 # move to upper
            else
              if (U<0.51) 
                mchain[i]=2 # move to middle
              else
                mchain[i]=1 # stay
          }
    if ((i%%10)==0) # every 10th sample
      {
        readline("re-draw every 10 steps\n")
        hist(mchain,probability=TRUE, breaks=seq(1,3,by=0.25),ylim=c(0.0,2.7)) #,"Histogram of 3-state Markov chain")
      }
  }

################################################
## look at sample means of indicator functions
## (= proportion of times in particular states)
################################################
sum(mchain==1)/N
sum(mchain==2)/N
sum(mchain==3)/N
##hist(mchain,probability=TRUE,breaks=seq(1,3,by=0.25),"Histogram of 3-state Markov chain")


################################################
## look at transition probability matrix
## P^n as n gets big
################################################

## stationary distribution example 4.19 Mobility table
tpm <- rbind(c(0.45, 0.48, 0.07),c(0.05, 0.70, 0.25), c(0.01, 0.50, 0.49))
## note: in library expm: tpm%^%2
tpm2 <- tpm%*%tpm
       [,1]   [,2]   [,3]
[1,] 0.2272 0.5870 0.1858
[2,] 0.0600 0.6390 0.3010
[3,] 0.0344 0.5998 0.3658
tpm5 <- tpm%*%tpm%*%tpm%*%tpm%*%tpm
           [,1]      [,2]      [,3]
[1,] 0.07581789 0.6221158 0.3020663
[2,] 0.06238685 0.6235696 0.3140435
[3,] 0.05972524 0.6234466 0.3168281
tpm10 <- tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm
           [,1]      [,2]      [,3]
[1,] 0.06260118 0.6234223 0.3139766
[2,] 0.06238891 0.6234403 0.3141708
[3,] 0.06234575 0.6234438 0.3142104

tpm20 <-tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm
           [,1]      [,2]      [,3]
[1,] 0.06238865 0.6234403 0.3141711
[2,] 0.06238859 0.6234403 0.3141711
[3,] 0.06238858 0.6234403 0.3141711

tpm40 <- tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm%*%tpm
           [,1]      [,2]      [,3]
[1,] 0.06238859 0.6234403 0.3141711
[2,] 0.06238859 0.6234403 0.3141711
[3,] 0.06238859 0.6234403 0.3141711


## calculating stationary distr.
pi0 <- 0.7/(20*0.56+0.02)
pi1 <- (20*0.56*pi0 - 0.2)/0.8
pi1
[1] 0.6234403
pi2 <- 1-pi0-pi1
pi2
[1] 0.3141711

pimat <- matrix(c(pi0,pi1,pi2),3,1)
pimat
           [,1]
[1,] 0.06238859
[2,] 0.62344029
[3,] 0.31417112

### similar to quantity from transition matrix above
pi0 <- 0.06238859
pi1 <- 0.6234403
pi2 <- 0.3141711

## example 1
tpm <- rbind(c(2/5, 1/2, 1/10),c(1/5, 7/10, 1/10), c(2/5, 2/5, 1/5))
tpmsq <- tpm%*%tpm
> tpmsq
     [,1] [,2] [,3]
[1,] 0.30 0.59 0.11
[2,] 0.26 0.63 0.11
[3,] 0.32 0.56 0.12
tpm4 <- tpmsq%*%tpmsq
       [,1]   [,2]   [,3]
[1,] 0.2786 0.6103 0.1111
[2,] 0.2770 0.6119 0.1111
[3,] 0.2800 0.6088 0.1112

tpm10 <- tpmsq%*%tpmsq%*%tpmsq%*%tpmsq%*%tpmsq%*%tpmsq%*%tpmsq%*%tpmsq%*%tpmsq%*%tpmsq
          [,1]      [,2]      [,3]
[1,] 0.2777778 0.6111111 0.1111111
[2,] 0.2777778 0.6111111 0.1111111
[3,] 0.2777778 0.6111111 0.1111111
> 18*tpm10
     [,1] [,2] [,3]
[1,]    5   11    2
[2,]    5   11    2
[3,]    5   11    2


