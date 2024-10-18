############################
## Probability Example 1
## Binomial(n, p)
## p is parameter of model
############################
M=1000
sim = rbinom(M, 100, 0.7) # generate M Bin(n=100,p=0.7) random variables
hist(sim) # approximate distribution of X ~ Bin(100,0.7)
sum(sim>70)/M # compute proportion > 70. Approximates P(X>70)

sim = rbinom(M, 100, 0.6) # generate M Bin(n=100,p=0.7) random variables
hist(sim) # approximate distribution of X ~ Bin(100,0.6)
sum(sim>70)/M # compute proportion > 70. Approximates P(X>70)
sum(sim>60)/M # compute proportion > 60. Approximates P(X>60)




