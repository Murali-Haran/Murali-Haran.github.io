##############################
### Poisson regression example
##############################
set.seed(1)
N=100
beta0=3
beta1=-1
beta2=1
sigmasq=1
x1=runif(N, 0, 1)
x2=runif(N, 0, 1)

y=rpois(N, exp(beta0+x1*beta1 + x2*beta2)) #+rnorm(N, 0, sqrt(sigmasq))
plot(x1, y, main="Simple linear regression simulation")
plot(x2, y, main="Simple linear regression simulation")

loglike=function(b1, b2){
    logval=0
    for (i in 1:length(y))
        logval= logval + y[i]*(beta0+x1[i]*b1[i]+x2[i]*b2[i]) - exp(beta0+x1[i]*b1[i]+x2[i]*b2[i])
    return(logval)
}
