################################
## Toy e.g. 
################################
n=2000
set.seed(1)
X=rexp(n, 1/11)## "real" data
B=1000
meanBS=rep(NA, B)
for (i in 1:B){
    foo=sample(X, size=n, replace=TRUE)
    meanBS[i]=mean(foo)
}
plot(density(meanBS), main="Bootstrap approximation of sampling distribution of mean")

################################
## I know the true population
################################
meanTrue=rep(NA, B)
for (i in 1:B){
    foo=rexp(n, 1/11)
    meanTrue[i]=mean(foo)
}
lines(density(meanTrue), main="Bootstrap approximation of sampling distribution of mean", col="blue")


##set.seed(1)

#ts.sim <- arima.sim(list(order = c(1,0,1), ar = c(0.4), ma=c(0.2)), n = 200 )
## Read in time series (actually simulated according to above)

Y=scan("http://personal.psu.edu/muh10/540/data/armall.dat")
n=length(Y)
## fit arima model
fitmod = arima(Y, order=c(1,0,1), transform.pars=TRUE)

## Now parametric bootstrap with  multiple TS with fitted ARIMA model

B = 1000 # number of bootstrap replicates
matrixBS = matrix(NA, B, 3) # matrix with B rows and 3 columns (phi1, theta1, sigma2), initally filled with NA (junk)

for (i in 1:B)
  {
    timeseriesBS =arima.sim(list(order = c(1,0,1), ar = c(fitmod$coef[1]), ma=c(fitmod$coef[2])), sd=sqrt(fitmod$sigma2), n = 200 ) ## simulate TS
    fitmodBS=arima(timeseriesBS, order=c(1,0,1), transform.pars=TRUE)## fit TS
    matrixBS[i,]=c(fitmodBS$coef[1],fitmodBS$coef[2],fitmodBS$sigma2)
    
  }

hist(matrixBS[,1], main="Bootstrap sampling dist of phi1", xlab="phi1")
hist(matrixBS[,2], main="Bootstrap sampling dist of theta1", xlab="theta1")
hist(matrixBS[,3], main="Bootstrap sampling dist of sigma^2", xlab="sigma^2") 

### to obtain lower limit and upper limit of BS 95% confidence interval for phi1
quantile(matrixBS[,1], 0.025)
quantile(matrixBS[,1], 0.975)

### slicker way to do this
quantile(matrixBS[,1], c(0.025, 0.975))
quantile(matrixBS[,2], c(0.025, 0.975))
