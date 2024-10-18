## Here is some simple code in R that simulates according to the transition matrix
## of Example 4.3 (about Gary being cheerful, so-so, or glum).
## 
## Starting from an initial distribution of pi_0=(1/3, 1/3, 1/3), the code will 
## run a chain for 10000 time steps, then summarize the number of steps spent in
## each of the three states.

pi0 <- c(1/3, 1/3, 1/3) #Initialize pi0 vector
P <- matrix(c(.5, .3, .2, .4, .4, .3, .1, .3, .5), 3, 3) # Initialize transition matrix

## We'll use the 'sample' function a lot.  Check out R's help file on it by typing:
help(sample)

## Here we go.  First, generate X0:
X <- sample(3, 1, prob=pi0)
currentX <- X

## Now, repeat 10,000 times:
for (i in 1:10000) {
  newX <- sample(3, 1, prob=P[currentX,]) # Use as prob vector the currentX row of P
  X <- c(X, newX) # append newX to end of X (to store its value)
  currentX <- newX # get ready for next iteration
}

## Finally, we'd like to summarize the X vector.  Use the 'table' function for this:
table(X)


