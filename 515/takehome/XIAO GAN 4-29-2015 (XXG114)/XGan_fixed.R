#1 

################ Preparations ##################

#################################################
## pdf of EMG
## set log=TRUE to obtain pdf in log-scale
#################################################
dexpgauss<- function (x, mu = 0, sigma = 1, lambda = 1, log = FALSE)
{
    l <- max(length(x), length(mu), length(sigma), length(lambda))
    x <- rep(x, times = ceiling(l/length(x)), length.out = l)
    mu <- rep(mu, times = ceiling(l/length(mu)), length.out = l)
    sigma <- rep(sigma, times = ceiling(l/length(sigma)), length.out = l)
    lambda <- rep(lambda, times = ceiling(l/length(lambda)),
        length.out = l)
    if (min(sigma) <= 0) {
        stop("Sigma must be greater than zero")
    }
    if (min(lambda) <= 0) {
        stop("Lambda must be greater than zero")
    }
    erfc <- pnorm((mu + lambda * sigma * sigma - x)/sigma, lower.tail = FALSE,
        log.p = log)
    if (log) {
        result <- lambda/2 * (2 * mu + lambda * sigma * sigma -
            2 * x) + Re(erfc) + log(lambda)
    }
    else {
        result <- exp(lambda/2 * (2 * mu + lambda * sigma * sigma -
            2 * x)) * Re(erfc) * lambda
    }
    result[is.nan(result)] <- 0
    result
}

########################################
## function to plot pdf
## E.g. plotdexpgauss(-4,6,0,1,1)
########################################
plotdexpgauss <- function(lowerlim, upperlim, mu, sigma, lambda, log=FALSE)
{
  xvals=seq(lowerlim, upperlim, length=100)
  tempfun = function(x) return(dexpgauss(x, mu,sigma, lambda, log))
  yvals=sapply(xvals, tempfun)
  if (log)
    plot(xvals, yvals, type="l", main=paste("log(pdf) of Exp Modified Gaussian (mu=",mu,", sigma=",sigma,", lambda=",lambda), xlab="x", ylab="")
  else
    plot(xvals, yvals, type="l", main=paste("Exp Modified Gaussian mu=",mu,"sigma=",sigma,",lambda=",lambda), xlab="x", ylab="")
}
  
########################################
## EMG random variate generation
########################################
rexpgauss <- function(n, mu, sigma, lambda)
  {
    samp <- rnorm(n, mu, sigma)+rexp(n,lambda) 
    return(samp)
  }

## plot how Monte Carlo estimates change with increase in sample size
## input: samp (sample vector) and g (where E(g(x)) is quantity of interest)
## output: plot of estimate over time (increasing sample size)
## e.g.: estvssamp(outp,plotname=expression(paste("E(", beta,")")))
estvssamp = function(samp, g=mean, plotname="mean estimates")
  {
    if (length(samp)<100)
      batchsize = 1
    else
      batchsize = length(samp)%/%100

    est = c()
    for (i in seq(batchsize,length(samp),by=batchsize))
      {
        est = c(est, g(samp[1:i]))
      }

#    plot(seq(batchsize,length(samp),by=batchsize),est,main=paste("M.C. estimates vs. sample size\n"),type="l",xlab="sample size",ylab="MC estimate")
    plot(seq(batchsize,length(samp),by=batchsize),est,main=plotname,type="l",xlab="sample size",ylab="MC estimate")
  }

bm <- function(vals,bs="sqroot",warn=FALSE)
  {
    N <- length(vals)
    if (N<1000)
      {
        if (warn) # if warning
          cat("WARNING: too few samples (less than 1000)\n")
        if (N<10)
          return(NA)
      }

    if (bs=="sqroot") 
      {
        b <- floor(sqrt(N)) # batch size
        a <- floor(N/b) # number of batches
      }
    else
      if (bs=="cuberoot") 
        {
          b <- floor(N^(1/3)) # batch size
          a <- floor(N/b) # number of batches
        }
    else # batch size provided
      {
        stopifnot(is.numeric(bs))  
        b <- floor(bs) # batch size
        if (b > 1) # batch size valid
          a <- floor(N/b) # number of batches
        else
          stop("batch size invalid (bs=",bs,")")
      }
    
    Ys <- sapply(1:a,function(k) return(mean(vals[((k-1)*b+1):(k*b)])))

    muhat <- mean(Ys)
    sigmahatsq <- b*sum((Ys-muhat)^2)/(a-1)

    bmse <- sqrt(sigmahatsq/N)

    return(list(est=muhat,se=bmse))
  }
bmse <- function(vals,bs="sqroot",warn=FALSE)
  {
    N <- length(vals)
    if (N<1000)
      {
        if (warn) # if warning
          cat("WARNING: too few samples (less than 1000)\n")
        if (N<10)
          return(NA)
      }

    if (bs=="sqroot") 
      {
        b <- floor(sqrt(N)) # batch size
        a <- floor(N/b) # number of batches
      }
    else
      if (bs=="cuberoot") 
        {
          b <- floor(N^(1/3)) # batch size
          a <- floor(N/b) # number of batches
        }
    else # batch size provided
      {
        stopifnot(is.numeric(bs))  
        b <- floor(bs) # batch size
        if (b > 1) # batch size valid
          a <- floor(N/b) # number of batches
        else
          stop("batch size invalid (bs=",bs,")")
      }
    
    Ys <- sapply(1:a,function(k) return(mean(vals[((k-1)*b+1):(k*b)])))

    muhat <- mean(Ys)
    sigmahatsq <- b*sum((Ys-muhat)^2)/(a-1)

    bmse <- sqrt(sigmahatsq/N)

    return(bmse)
  }

#plotdexpgauss(0, 20, 5, 1, 0.4, log=FALSE)
#Output: centered at ~6



## estimate effective sample size (ESS) as described in Kass et al (1998) and Robert and Casella (2004; pg.500)
## ESS=size of an iid sample with the same variance as the current sample
## ESS = T/kappa  where kappa (the `autocorrelation time' for the sample) = 1 + 2 sum of all lag auto-correlations
## Here we use a version analogous to IMSE where we cut off correlations beyond a certain lag (to reduce noise)
ess = function(outp,imselags=TRUE)
  {
    if (imselags) # truncate number of lags based on imse approach
      {
        chainACov <- acf(outp,type="covariance",plot = FALSE)$acf ## USE AUTOCOVARIANCES
        ACovlen <- length(chainACov)
        gammaACov <- chainACov[1:(ACovlen-1)]+chainACov[2:ACovlen]
        
        m <- 1
        currgamma <- gammaACov[1]
        k <- 1
        while ((k<length(gammaACov)) && (gammaACov[k+1]>0) && (gammaACov[k]>=gammaACov[k+1]))
          k <- k +1
        cat("truncated after ",k," lags\n")
        if (k==length(gammaACov)) # added up until the very last computed autocovariance
          cat("WARNING: may need to compute more autocovariances/autocorrelations for ess\n")

        chainACorr = acf(outp,type="correlation",plot = FALSE)$acf ## USE AUTOCORRELATIONS
        if (k==1)
          ACtime = 1
        else
          ACtime <- 1 + 2*sum(chainACorr[2:k])  # add autocorrelations up to lag determined by imse
      }
    else
      {
        chainACorr = acf(outp,type="correlation",plot = FALSE)$acf ## USE AUTOCORRELATIONS
        ACtime <- 1 + 2*sum(chainACorr[-c(1)])
      }
    
    return(length(outp)/ACtime)
  }

## effective samples per second
espersec = function(outp,numsec,imselags=TRUE)
  {
    essval = ess(outp,imselags)
    return(essval/numsec)
}



###################### start of solutions ######################
############### Prob. 1 #################


#1
#parameters
beta0=5
lam=0.4
sig=1   #sigma_i=1 for all i. Since we don't change it throughout this exam, I'm using a number instead of a vector.


data = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
ys <- data[,-c(1)]
xs <- data[,-c(2)]
#beta1=2

# the posterior distribution function (target function) in log scale
logh <- function(beta0,beta1,xs,sig,lam,ys){
	ysp <- mapply(dexpgauss,ys,mu=beta0+beta1*xs, sigma = sig, lambda = lam, log=TRUE) #record the pmf for each y in ys
	return (sum(ysp)+dnorm(beta1,0,10,log=TRUE))
}
#ysp <- mapply(dexpgauss,ys,mu=beta0+beta1*xs, sigma = sig, lambda = lam, log=TRUE)
#ysp
#xs

n=10000 # Monte Carlo sample size



i=2
a=0 # store number of accepted samples
r=0 # store number of rejected samples

A = rep(NA, n) # The sample

A[1] = 5 # start from a random A[1]

tau1=1 #varicance for q(), the proposal distr.

while(i<=n){
	q = rnorm(1,A[i-1],tau1) #generate a candidate
	p = runif(1,0,1) #generate p to determine whether a candidate is accepted.
	k = logh(beta0,q,xs,sig,lam,ys)-logh(beta0,A[i-1],xs,sig,lam,ys)
	if (p<=(min(1,exp(k)))){
		A[i] = q
		a=a+1
	}
	else{
		A[i]=A[i-1]
		r=r+1
	}
	i=i+1
}
A
a/(a+r) #acceptence rate
plot(density(A))
bm(A)

##### plotting MCMCse ######

m <- seq(100, n, by = 100)
seA= rep(NA,length(m))

for (i in 1:length(m)){
	seA[i]=bmse(A[1:m[i]])  # bmse just gives MCMCse of a vector
}
plot(m,seA,main="MCMCse with sample size", xlab="sample size", ylab="MCMCse")

quantile(A,c(0.025, 0.975))

acf(A)

estvssamp(A,g=mean,plotname="mean estimates")




############### Prob. 2 #################


#2(b)
#parameters
sig=1   #sigma_i=1 for all i. Since we don't change it throughout this exam, I'm using a number instead of a vector.


data = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
ys <- data[,-c(1)]
xs <- data[,-c(2)]


# the posterior distribution function (target function) in log scale
logh <- function(beta0,beta1,xs,sig,lam,ys){
	ysp <- mapply(dexpgauss,ys,mu=beta0+beta1*xs, sigma = sig, lambda = lam, log=TRUE) #record the pmf for each y in ys
	return (sum(ysp)+dnorm(beta1,0,10,log=TRUE)+dnorm(beta0,0,10,log=TRUE)+dgamma(lam, shape=0.01,scale=100,log=TRUE))
}




n=10000 # Monte Carlo sample size



i=2
a1=0 # store number of accepted samples for beta1
r1=0 # store number of rejected samples for beta1
a2=0 # store number of accepted samples for beta0
r2=0 # store number of rejected samples for beta0
a3=0 # store number of accepted samples for lambda
r3=0 # store number of rejected samples for lambda




A = rep(NA, n) # The sample for beta1
B = rep(NA, n) # The sample for beta0
C = rep(NA, n) # The sample for lambda

A[1] = 4 # start from a random state
B[1] = 2
C[1] = 0.5


while(i<=n){
	####### sample beta1 ########
	q = rnorm(1,A[i-1],0.5) #generate a candidate for beta1
	p = runif(1,0,1) #generate p to determine whether a candidate is accepted.
	k = logh(B[i-1],q,xs,sig,C[i-1],ys)-logh(B[i-1],A[i-1],xs,sig,C[i-1],ys)
	if (p<=(min(1,exp(k)))){
		A[i] = q
		a1=a1+1
	}
	else{
		A[i]=A[i-1]
		r1=r1+1
	}
	########## sample beta0 #######
	q = rnorm(1,B[i-1],0.4) #generate a candidate for beta0
	p = runif(1,0,1) #generate p to determine whether a candidate is accepted.
	k = logh(q,A[i],xs,sig,C[i-1],ys)-logh(B[i-1],A[i],xs,sig,C[i-1],ys)
	if (p<=(min(1,exp(k)))){
		B[i] = q
		a2=a2+1
	}
	else{
		B[i]=B[i-1]
		r2=r2+1
	}
	########## sample lambda #######
		q = runif(1,0.0001,2)
		p = runif(1,0,1) #generate p to determine whether a candidate is accepted.
		k = logh(B[i],A[i],xs,sig,q,ys)-logh(B[i],A[i],xs,sig,C[i-1],ys)
		if (p<=(min(1,exp(k)))){
			C[i] = q
			a3=a3+1
		}
		else{
			C[i]=C[i-1]
			r3=r3+1
		}

	i=i+1
}



a1/(a1+r1) #acceptence rate for beta1
plot(density(A))
bm(A)
quantile(A,c(0.025, 0.975))

##### plotting MCMCse ######

m <- seq(100, n, by = 100)
seA= rep(NA,length(m))

for (i in 1:length(m)){
	seA[i]=bmse(A[1:m[i]])  # bmse just gives MCMCse of a vector
}
plot(m,seA,main="MCMCse with sample size", xlab="sample size", ylab="MCMCse")



a2/(a2+r2) #acceptence rate for beta0
plot(density(B))
bm(B)
quantile(B,c(0.025, 0.975))

m <- seq(100, n, by = 100)
seB= rep(NA,length(m))

for (i in 1:length(m)){
	seB[i]=bmse(B[1:m[i]])  # bmse just gives MCMCse of a vector
}
plot(m,seB,main="MCMCse with sample size", xlab="sample size", ylab="MCMCse")



a3/(a3+r3) #acceptence rate for lambda\
plot(density(C))
bm(C)
quantile(C,c(0.025, 0.975))

m <- seq(100, n, by = 100)
seC= rep(NA,length(m))

for (i in 1:length(m)){
	seC[i]=bmse(C[1:m[i]])  # bmse just gives MCMCse of a vector
}
plot(m,seC,main="MCMCse with sample size", xlab="sample size", ylab="MCMCse")


cor(A,B)  #correlation between Beta1 and Beta2

acf(A)
acf(B)
acf(C)

estvssamp(A,g=mean,plotname="A mean estimates")
estvssamp(B,g=mean,plotname="B mean estimates")
estvssamp(C,g=mean,plotname="C mean estimates")


#################### prob.3 #########################

#parameters
sig=1   #sigma_i=1 for all i. Since we don't change it throughout this exam, I'm using a number instead of a vector.


data = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")
ys <- data[,-c(1)]
xs <- data[,-c(2)]


# the posterior distribution function (target function) for beta1 in log scale
logh1 <- function(beta0,beta1,xs,sig,lam,ys){
	ysp <- mapply(dexpgauss,ys,mu=beta0+beta1*xs, sigma = sig, lambda = lam, log=TRUE) #record the pmf for each y in ys
	return (sum(ysp)+dnorm(beta1,0,10,log=TRUE))
}
# the posterior distribution function (target function) for beta0 in log scale
logh0 <- function(beta0,beta1,xs,sig,lam,ys){
	ysp <- mapply(dexpgauss,ys,mu=beta0+beta1*xs, sigma = sig, lambda = lam, log=TRUE) #record the pmf for each y in ys
	return (sum(ysp)+dnorm(beta0,0,10,log=TRUE))
}

# the posterior distribution function (target function) for lambda in log scale
loghlam <- function(beta0,beta1,xs,sig,lam,ys){
	ysp <- mapply(dexpgauss,ys,mu=beta0+beta1*xs, sigma = sig, lambda = lam, log=TRUE) #record the pmf for each y in ys
	return (sum(ysp)+dgamma(lam, shape=0.01,scale=100,log=TRUE))
}



n=1000 # Monte Carlo sample size
z <- Sys.time()


i=2
a1=0 # store number of accepted samples for beta1
r1=0 # store number of rejected samples for beta1
a2=0 # store number of accepted samples for beta0
r2=0 # store number of rejected samples for beta0
a3=0 # store number of accepted samples for lambda
r3=0 # store number of rejected samples for lambda




A = rep(NA, n) # The sample for beta1
B = rep(NA, n) # The sample for beta0
C = rep(NA, n) # The sample for lambda

A[1] = 3 # start from a random state
B[1] = 0.2
C[1] = 0.15


while(i<=n){
	####### sample beta1 ########
	q = rnorm(1,A[i-1],0.2) #generate a candidate for beta1
	p = runif(1,0,1) #generate p to determine whether a candidate is accepted.
	k = logh1(B[i-1],q,xs,sig,C[i-1],ys)-logh1(B[i-1],A[i-1],xs,sig,C[i-1],ys)
	if (p<=(min(1,exp(k)))){
		A[i] = q
		a1=a1+1
	}
	else{
		A[i]=A[i-1]
		r1=r1+1
	}
	########## sample beta0 #######
	q = rnorm(1,B[i-1],0.15) #generate a candidate for beta0
	p = runif(1,0,1) #generate p to determine whether a candidate is accepted.
	k = logh0(q,A[i],xs,sig,C[i-1],ys)-logh0(B[i-1],A[i],xs,sig,C[i-1],ys)
	if (p<=(min(1,exp(k)))){
		B[i] = q
		a2=a2+1
	}
	else{
		B[i]=B[i-1]
		r2=r2+1
	}
	########## sample lambda #######
	q=0
	while (q<=0 | q>=2*C[i-1]){
		q = rnorm(1,C[i-1],0.01) #generate a candidate for lambda
	}
		p = runif(1,0,1) #generate p to determine whether a candidate is accepted.
		k = loghlam(B[i],A[i],xs,sig,q,ys)-loghlam(B[i],A[i],xs,sig,C[i-1],ys)
		if (p<=(min(1,exp(k)))){
			C[i] = q
			a3=a3+1
		}
		else{
			C[i]=C[i-1]
			r3=r3+1
		}

	i=i+1
}
z
Sys.time()


a1/(a1+r1) #acceptence rate for beta1
plot(density(A),main="Density plot of Beta1")
bm(A)
quantile(A,c(0.025, 0.975))


a2/(a2+r2) #acceptence rate for beta0
plot(density(B),main="Density plot of Beta0")
bm(B)
quantile(B,c(0.025, 0.975))

a3/(a3+r3) #acceptence rate for lambda\
plot(density(C),main="Density plot of lambda")
bm(C)
quantile(C,c(0.025, 0.975))


acf(A)
acf(B)
acf(C)

estvssamp(A,g=mean,plotname="A mean estimates")
estvssamp(B,g=mean,plotname="B mean estimates")
estvssamp(C,g=mean,plotname="C mean estimates")


m <- seq(100, n, by = 100)
seA= rep(NA,length(m))
seB= rep(NA,length(m))
seC= rep(NA,length(m))

for (i in 1:length(m)){
	seA[i]=bmse(A[1:m[i]])  # bmse just gives MCMCse of a vector
	seB[i]=bmse(B[1:m[i]])
	seC[i]=bmse(C[1:m[i]])
}
plot(m,seA,main="Beta1 MCMCse with sample size", xlab="sample size", ylab="MCMCse")
plot(m,seB,main="Beta0 MCMCse with sample size", xlab="sample size", ylab="MCMCse")
plot(m,seC,main="lambda MCMCse with sample size", xlab="sample size", ylab="MCMCse")
