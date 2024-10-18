############################
## Probability Example 2
## Simulate from bivariate Normal
## mean=0, var=1,4
## cov = 1
############################

##muvec=c(0,0)
covmat=matrix(c(1,1,1,4),2,2)
cholmat=t(chol(covmat))
x=rnorm(2,0,1)
x=cholmat%*%x
M=100000
simBivNorm=matrix(NA,M,2)
for (i in 1:M)
    {
        iidNorm=rnorm(2,0,1)
        simBivNorm[i,]=cholmat%*%iidNorm
    }
### check
var(simBivNorm[,1])
var(simBivNorm[,2])
cor(simBivNorm[,1],simBivNorm[,2])
