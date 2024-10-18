
### I made up some data for this demonstration
X=rnorm(100) # simulate (made up data) of X variables
Y=X*2 + 4# simulate (made up data) of Y variables

## now I create a vector of colors
## notice that this vector is as long as the vector of
## X's and the vector of Y's
colorLs=c(rep("red",30), rep("blue",30), rep("green",40))
## now draw a scatterplot of Y versus X, but with each
## of the 100 (X,Y) points colored according to the
## vector of colors in colorLs
plot(X, Y, col=colorLs)

### create another data set for demonstration
spend=c(rnorm(5, 100, 5), rnorm(5, 120,10)) ## amount of money spent on clothes per semester
age=sample(18:22, 10, replace=TRUE) ## ages 
gender=c(rep("m",5), rep("f",5)) 
colorLs=rep(NA, 10) # make up a vector of length 10 with NAs at the beginning
colorLs[gender=="m"]="blue"
colorLs[gender=="f"]="red"
plot(age, spend, col=colorLs)


####################################
## overlaying plots
####################################
normSamp=rnorm(50, mean=3,sd=4) # 50 N(3,4) random variables
gammaSamp=rgamma(50, shape=5, scale=2) # 50 Gamma r.v.s
plot(density(normSamp), col="red",main="Plots of N(3,4) and Gamma(5,2) samples")
lines(density(gammaSamp), col="blue")



####################################
## N(0,1) vs Gamma(shape=1, scale=2)
####################################
