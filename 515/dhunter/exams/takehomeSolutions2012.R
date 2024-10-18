### R code from vignette source 'takehomeSolutions2012'

###################################################
### code chunk number 1: takehomeSolutions2012.Rnw:125-138
###################################################
Pt=rbind( c(0, 6, 0, 0, 0, 0, 0, 0, 0),
                  c(4, 0, 4, 2, 0, 0, 0, 0, 0),
                  c(2, 3, 0, 3, 2, 1, 0, 0, 0),
                  c(1, 2, 3, 0, 3, 2, 1, 0, 0),
                  c(0, 1, 2, 3, 0, 3, 2, 1, 0),
                  c(0, 0, 1, 2, 3, 0, 3, 2, 1),
                  c(0, 0, 0, 1, 2, 3, 0, 3, 2),
                  c(0, 0, 0, 0, 0, 2, 4, 0, 2),
                  c(0, 0, 0, 0, 0, 0, 0, 6, 0)) / 6
Pt[row(Pt)>col(Pt)] <- Pt[row(Pt)>col(Pt)] * 244/495
Pt[row(Pt)<col(Pt)] <- Pt[row(Pt)<col(Pt)] * 251/495
S <- solve(diag(rep(1,9)) - Pt)
sum(S[5,])


###################################################
### code chunk number 2: takehomeSolutions2012.Rnw:247-269
###################################################
## First, consider theta0 (with sample size one million):
theta0 <- rnorm(n <- 1e6, mean=-.17, sd=1)
p0<- exp(theta0)/(1+exp(theta0))
a0 <- theta0 * dbinom(64, 140, p0) * dnorm(theta0) / dnorm(theta0, mean=-.17, sd=1)
b0 <- dbinom(64,140,p0) * dnorm(theta0) / dnorm(theta0, mean=-.17, sd=1)
meanTheta0 <- (muA0 <- mean(a0)) / (muB0 <- mean(b0))
meanTheta0

## Next, same thing for theta1:
theta1 <- rnorm(n, mean=1.61, sd=1)
p1<- exp(theta1)/(1+exp(theta1))
a1 <- theta1 * dbinom(50, 60, p1) * dnorm(theta1) / dnorm(theta1, mean=1.61, sd=1)
b1 <- dbinom(50, 60,p1) * dnorm(theta1) / dnorm(theta1, mean=1.61, sd=1)
meanTheta1 <- (muA1 <- mean(a1)) / (muB1 <- mean(b1))
meanTheta1

## Finally, for lambda:
lambda <- rbeta(n, 1.5, 3.5)
a2 <- lambda * dbeta(lambda, 4, 8) * dbeta(lambda, 2, 2) / dbeta(lambda, 1.5, 3.5)
b2 <- dbeta(lambda, 4, 8) * dbeta(lambda, 2, 2) / dbeta(lambda, 1.5, 3.5)
meanLambda <- (muA2 <- mean(a2)) / (muB2 <- mean(b2))
c(meanLambda, 5/14) # These values should be close!


###################################################
### code chunk number 3: takehomeSolutions2012.Rnw:297-309
###################################################
# For theta0:
tmp <- c(1, -muA0 / muB0)
var0 <- tmp %*% var(cbind(a0, b0)) %*% tmp / n / muB0^2
meanTheta0 + c(-1.96, 1.96) * sqrt(var0)
# For theta1:
tmp <- c(1, -muA1 / muB1)
var1 <- tmp %*% var(cbind(a1, b1)) %*% tmp / n / muB1^2
meanTheta1 + c(-1.96, 1.96) * sqrt(var1)
# For lambda:
tmp <- c(1, -muA2 / muB2)
var2 <- tmp %*% var(cbind(a2, b2)) %*% tmp / n / muB2^2
meanLambda + c(-1.96, 1.96) * sqrt(var2)


###################################################
### code chunk number 4: takehomeSolutions2012.Rnw:383-415
###################################################
# Initialization:
x <- c(18, 9, 12, 9, 14, 5, 18, 12, 8, 9)
z <- c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0) # The last five of these are arbitrary starting values
cumsumz6to10 <- rep(0, 5) # All we need to keep track of is the running total of the Z_j
iterations <- 1e6
theta0 <- theta1 <- rep(0, 1+iterations)
lambda <- rep(1/2, iterations)
for (i in 1:iterations) {
  # Step 1:  Update lambda
  lambda[i+1] <- rbeta(1, 1+sum(z), 11-sum(z))
  # Step 2:  Update theta0
  Proposal <- rnorm(1, mean=theta0[i])
  logMHRatio <- (theta0[i]^2 - Proposal^2)/2 + 
                          (Proposal - theta0[i]) * sum((1-z) * x) -
                          20 * sum(1-z) * log(1 + exp(Proposal)) +
                          20 * sum(1-z) * log(1 + exp(theta0[i]))
  theta0[i+1] <- ifelse (log(runif(1)) < logMHRatio, Proposal, theta0[i])
  # Step 3:  Update theta1
  Proposal <- rnorm(1, mean=theta1[i])
  logMHRatio <- (theta1[i]^2 - Proposal^2)/2 + 
                          (Proposal - theta1[i]) * sum(z * x) -
                          20 * sum(z) * log(1 + exp(Proposal)) +
                          20 * sum(z) * log(1 + exp(theta1[i]))
  theta1[i+1] <- ifelse (log(runif(1)) < logMHRatio, Proposal, theta1[i])
  # Step 4:  Update Z_6 through Z_10
  for (j in 6:10) {
    alpha <- exp( log(lambda[i]) + x[j] * theta1[i] - 20*log(1+exp(theta1[i])))
    beta <- exp( log(1-lambda[i]) + x[j] * theta0[i] - 20*log(1+exp(theta0[i])))
    z[j] <- rbinom(1, 1, alpha/(alpha+beta)) 
    cumsumz6to10[j-5] <- cumsumz6to10[j-5] + z[j] # Needed to find posterior means
  }
}


###################################################
### code chunk number 5: takehomeSolutions2012.Rnw:419-423
###################################################
par(mfrow=c(2,2))
plot(theta0[(1:1000)*iterations/1000], type="l")
plot(theta1[(1:1000)*iterations/1000], type="l")
plot(lambda[(1:1000)*iterations/1000], type="l")


###################################################
### code chunk number 6: takehomeSolutions2012.Rnw:439-442
###################################################
inverseLogit <- function(x) exp(x) / (1+exp(x))
inverseLogit(quantile(theta0, c(.025, .975))) # p0 credible interval
inverseLogit(quantile(theta0, c(.025, .975))) # p1 credible interval


###################################################
### code chunk number 7: takehomeSolutions2012.Rnw:451-456
###################################################
phat <- cumsumz6to10 / (1+iterations)
phat # These are the estimated posterior means
lowerBounds <- phat - 1.96 * sqrt(phat * (1-phat) / (1 + iterations))
upperBounds <- phat + 1.96 * sqrt(phat * (1-phat) / (1 + iterations))
rbind(lowerBounds, upperBounds) # There are the conf intervals


