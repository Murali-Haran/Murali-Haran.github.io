## Here is some code in R that could be modified for problem 1(a) and (b) on 
## homework #8.  The idea is that we iteratively recalculate some matrix,
## called 'answer', until the changes between successive calculations differ
## by no more than 1e-5 in each matrix entry.

lastAnswer <- matrix(0, 5, 5) # Initialize the matrix to be compared with the 
                              # 'answer' matrix.
finished <- FALSE

# Here, insert any initialization that must happen before your main loop
k <- 0 # Let's at least keep track of how many iterations are required

while (!finished) {
  k <- k+1 

  # Here, insert calculation of the value of the 'answer' matrix for 
  # the kth iteration
  
  finished <- all(abs(answer - lastAnswer) < 1e-5) # check for convergence
  lastAnswer <- answer # Save answer to check in next iteration
}

# Finally, output the final answer as well as the number of iterations:
lastAnswer
k

