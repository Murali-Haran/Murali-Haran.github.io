## Here is some R code to illustrate the use of lists,
## which can store arbitrary sets of objects (e.g., vectors of
## different lengths, as in this example)

## Create a list of vectors.  Each vector will have a random (binomial)
## number of elements.  Each element will be standard normal.
vectors <- list()
for (i in 1:100) {
  n <- rbinom(1, 50, .5)
  vectors[[i]] <- rnorm(n)
}

## Now find the sample standard deviation of each list item.  Use the 
## sapply function, which applies the same function to each list item
## and returns a vector summarizing the results:
sapply (vectors, sd)

## If we want to count the number of vector elements in the range (0,1)
## for each vector, we can use sapply for that as well:
f <- function(vec) sum(0<vec & vec<1)
sapply(vectors, f)

## If we want, it's possible to combine the last two lines into one
sapply(vectors, function(vec) sum(0<vec & vec<1))

