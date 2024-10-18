## Here is some R code to illustrate the Poisson approximation to the binomial
## with small p and large n.
## It plots a histogram of a Poisson sample, then overlays the binomial pmf on 
## the same histogram so that the two may be compared.

# Initialize parameters
n <- 50
p <- 0.05
lambda <- n*p
samplesize <- 20000

# Now the Poisson sample and histogram
x <- rpois(samplesize, lambda) # draw the random sample
breaks <- ((min(x)-1):max(x)) + 0.5 # Optional:  Explicitly set the histogram breaks
hist(x, breaks = breaks, main = "")

# Now add the binomial lines for comparison
vals <- min(x):min(n, max(x)) # technically, the binomial can't go higher than n
lines (vals, samplesize*dbinom(vals, n, p), # dbinom is the binomial mass function
       col = 2, # make the lines red
       lwd = 2, # change line width to make the lines more visible
       lty = 2, # change line type to dotted
       type= "b") # type "b" means o--o--o--o style

# Add main title (could have done this with 'main' argument in hist function)
title("Poisson histogram with binomial overlay")



