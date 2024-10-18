## a function for computing batch means in R
## input: vals, a vector of N values (from a Markov chain) and g, a function
## output: estimate of E(g(x)) and an estimate of the Monte Carlo standard error of estimate of E(g(x))
id <- function(x) return(x)  # default: identity function

bm <- function(vals,g=id)
  {
    N <- length(vals)
    if (N<1000)
      {
        cat("WARNING: too few samples (less than 1000)\n")
        if (N<10)
          return(NA)
      }
    
    b <- floor(sqrt(N)) # batch size
    a <- floor(N/b)

    Ys <- rep(NA,a)
    for (k in 1:a)
      Ys[k] <- sum(g(vals[((k-1)*b+1):(k*b)]))/b

    muhat <- mean(g(vals))
    sigmahatsq <- b*sum((Ys-muhat)^2)/(a-1)

##    bmse <- sigmahatsq/N
    bmse <- sqrt(sigmahatsq/N)

    return(c(muhat,bmse))
  }
