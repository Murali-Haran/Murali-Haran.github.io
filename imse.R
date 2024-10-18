## Geyer's initial monotone positive sequence estimator
## input: Markov chain output (vector)
## output: monte carlo standard error estimate for chain
imse <- function(outp,asymvar=FALSE)
  {
    chainAC <- acf(outp,type="covariance",plot = FALSE)$acf ## USE AUTOCOVARIANCES
    AClen <- length(chainAC)
    gammaAC <- chainAC[1:(AClen-1)]+chainAC[2:AClen]

    m <- 1
    currgamma <- gammaAC[1]
    k <- 1
    while ((k<length(gammaAC)) && (gammaAC[k+1]>0) && (gammaAC[k]>=gammaAC[k+1]))
      k <- k +1

    if (k==length(gammaAC)) # added up until the very last computed autocovariance
      cat("WARNING: may need to compute more autocovariances for imse\n")
    sigmasq <- -chainAC[1]+2*sum(gammaAC[1:k])

    if (asymvar) # return asymptotic variance
      return(sigmasq)
    
    mcse <- sqrt(sigmasq/length(outp))
    return(mcse)
  }
