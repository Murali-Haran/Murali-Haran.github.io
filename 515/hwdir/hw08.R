##### plot intensity functions
xs = seq(0,10)
lambda = function(x)
  {
    max(0.2,sin(x)*3)
##    3*exp(-(x-3)^1.8) + 3*exp(-(x-5)^1.2)
  }
xs = seq(0,10)
plot(xs, sapply(xs, lambda), ylim=c(0,4))


