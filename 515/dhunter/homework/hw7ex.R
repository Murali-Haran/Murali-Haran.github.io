## Here is some code in R that simulates 10000 independent realizations of 
## a 4-state continuous-time Markov chain until time t=1.  We presume that
## we are given the rate matrix, R, and ultimately we want to find the proportion
## of the time spent in each of the four states.

# First, let's set up the R matrix (these numbers aren't actually meaningful;
# this is just an example)
R <- matrix(1:16, 4, 4); diag(R) <- 0; diag(R) <- -rowSums(R)
print(R) # Take a look at R

X <- list()  # Each item in X will consist of TWO vectors:  times and states
             # Actually, technically, each item will be a list with two elements:
             # A vector named times and a vector named states.
maxTime <- 1 # This is the cutoff time.
for (count in 1:10000) {
  times <- 0
  states <- 1 # Assume that we always start in state 1 at time 0
  i <- 1 # which time/state are we currently in
  finished <- FALSE # We'll set this to TRUE when it's time to stop.
  while (!finished) {
    currentState <- states[i]
    currentTime <- times[i]
    deltaTime <- rexp(1, rate = -R[currentState, currentState])
    if (currentTime + deltaTime > maxTime) {
      # Now we need to finish this chain
      deltaTime <- maxTime - currentTime
      finished <- TRUE
    }
    times <- c(times, currentTime + deltaTime)
    possibleMoves <- (1:4)[-currentState]
    states <- c(states, sample(possibleMoves, 1, prob=R[currentState,possibleMoves]))
    i <- i+1
  }
  X[[count]] <- list(times=times, states=states)
}

# Next, we need to summarize the time spent in each state.
# Define a function that takes a list with named items 'times' and 'states'
# then returns a vector giving total time spent in each state.
f <- function(a, nstates=4) { 
  deltaTimes <- diff(a$times)
  states <- a$states[-length(a$states)] # delete the final state, which is irrelevant
  sapply(1:nstates, function(b) sum(deltaTimes[states==b]))
}

# Next, apply the above function to each of the items in X:
timesPerState <- sapply(X, f)

# If we want, double-check that the total time (rounded to 12 digits, say) 
# sums to maxTime for each M.C.:
all(round(colSums(timesPerState),12)==maxTime)  # Should get TRUE if things are okay

# Calculate the average time spent in each of the four states:
rowMeans(timesPerState)

# Express the above as a proportion of the total time:
rowMeans(timesPerState) / maxTime

# How do these proportions compare with the limiting proportions? 
# The limiting proportions are a left-eigenvector of R having eigenvalue
# zero.  To get left-eigenvectors, just find eigenvectors of t(R):
e <- eigen(t(R))
pi <- e$vec[,e$val==0]
pi <- pi/sum(pi) # Need to standardize
print(pi)

