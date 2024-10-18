# Exercise 6:
N <- 1:1000
for(i in 1:1000) {
  X <- rpois(1,10)
  mytime <- runif(1)
  N[i] <- sum(runif(X)<mytime)
}
print(mean(N))
print(var(N))



