##############################
### toy least squares example
##############################
set.seed(1)
N=100
beta1=-1
beta2=1
sigmasq=1
x1=runif(N, 0, 1)
x2=runif(N, 0, 1)

y=x1*beta1 + x2*beta2 +rnorm(N, 0, sqrt(sigmasq))
plot(x1, y, main="Simple linear regression simulation")
plot(x2, y, main="Simple linear regression simulation")

objfun=function(b1, b2){
    ssq=sum((y-x1*b1-x2*b2)^2)
    return(ssq)
}
##objfun2=Vectorize(objfun, SIMPLIFY = FALSE)
##z=outer(bvals1, bvals2, objfun2)
##persp(bvals1,bvals2,unlist(z), col='blue')

objfun=function(b1, b2){
    ssq=0
    for (i in 1:length(y))
        ssq=ssq+ (y[i]-x1[i]*b1-x2[i]*b2)^2
    return(ssq)
}

bvals1=seq(-3,3, length=100)
bvals2=seq(-3,3, length=100)
##z=outer(bvals1, bvals2, objfun)
##z=outer(bvals1, bvals2, "*")
z=outer(bvals1, bvals2, objfun)
persp(bvals1,bvals2,z, col='blue')
Sys.sleep(10)#readline("enter for image plot ")
image(bvals1,bvals2,z)


##########################################
### gradient descent algorithm
##########################################
contour(bvals1,bvals2,z)
points(beta1,beta2, pch=19, col="red")
NUMIT=100
est=c(-1,-2) # initial value
sRate=1 #1.2 2, 0.5
points(est[1], est[2], pch=19, col="blue")
print(est)
for (i in 1:NUMIT){
    df1=-(2/N)*sum(x1*(y-est[1]*x1-est[2]*x2))
    df2=-(2/N)*sum(x2*(y-est[1]*x1-est[2]*x2))
##    df1=-2*sum(x1*(y-est[1]*x1-est[2]*x2))
##    df2=-2*sum(x2*(y-est[1]*x1-est[2]*x2))
    
    est[1]=est[1]-sRate*df1
    est[2]=est[2]-sRate*df2
    cat(paste("i=",i,", est=(",est[1],",",est[2],") \n"))
    points(est[1], est[2], pch=19, col="blue")
    ##    points(est[1], est[2], pch=paste(i), col="blue")
    readline("next iteration")
    ##    text(est[1],est[2],label=i,col="blue)
}
    
##########################################
### stochastic gradient descent algorithm
##########################################
set.seed(1)
contour(bvals1,bvals2,z)
points(beta1,beta2, pch=19, col="red")
NUMIT=500
est=c(1,1) # initial value
##est=c(-1,-2) # initial value
sRate= 3 #2 1 0.5 5
points(est[1], est[2], pch=19, col="blue")
print(est)
for (i in 1:NUMIT){
    randInd= sample(seq(1,N), 1)
    df1=-(2/N)*sum(x1[randInd]*(y[randInd]-est[1]*x1[randInd]-est[2]*x2[randInd]))
    df2=-(2/N)*sum(x2[randInd]*(y[randInd]-est[1]*x1[randInd]-est[2]*x2[randInd]))
    
    est[1]=est[1]-sRate*df1
    est[2]=est[2]-sRate*df2
    cat(paste("i=",i,", est=(",est[1],",",est[2],") \n"))
    points(est[1], est[2], pch=19, col="blue")
    ##    points(est[1], est[2], pch=paste(i), col="blue")
    readline("next iteration")
    ##    text(est[1],est[2],label=i,col="blue)
}
    
##########################################
## Newton-Raphson
##########################################
contour(bvals1,bvals2,z)
points(beta1,beta2, pch=19, col="red")
NUMIT=500
est=c(1,1) # initial value
##est=c(-1,-2) # initial value
sRate= 0.5 #2 1 0.5 5
points(est[1], est[2], pch=19, col="blue")
print(est)
for (i in 1:NUMIT){
    randInd= sample(seq(1,N), 1)
    df1=-(2/N)*sum(x1[randInd]*(y[randInd]-est[1]*x1[randInd]-est[2]*x2[randInd]))
    df2=-(2/N)*sum(x2[randInd]*(y[randInd]-est[1]*x1[randInd]-est[2]*x2[randInd]))

    df11=2*sum(x1^2)/N
    df22=2*sum(x2^2)/N
    df12=2*sum(x1*x2)/N
    hess=matrix(c(df11,df12,df12,df22), 2,2)

    est=est-sRate*solve(hess)%*%c(df1, df2)
    cat(paste("i=",i,", est=(",est[1],",",est[2],") \n"))
    points(est[1], est[2], pch=19, col="blue")
    ##    points(est[1], est[2], pch=paste(i), col="blue")
    readline("next iteration")
    ##    text(est[1],est[2],label=i,col="blue)
}
