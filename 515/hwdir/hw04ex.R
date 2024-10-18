###############################################################
## example code for simulating a branching process
###############################################################
## generate next generation for a single individual
## with p0=1/4, p1=1/4, p2=1/2
## test code
foo = rep(NA,100000)
for (i in 1:100000)
  foo[i] = sample(c(0,1,2), 1,replace=FALSE, prob=c(1/4, 1/4, 1/2))



NUMBRANCHPR = 10 # number of branching processes simulated
MAXGENS = 10000 # for computational convenience (this introduces a bias in the generation process)

extinction = rep(0, NUMBRANCHPR) # keep track of whether the process went extinct. 0=> did not go extinct, 1=> went extinct
processLen = rep(NA, NUMBRANCHPR) # keep track of length of processes generated

for (j in 1:NUMBRANCHPR) # number of branching process realizations
  {
    currgen = 1 # start X_0 at 1
    numgen = 0 # number of generations
    ## keep repeating until the size of the generation is 0 and number of generations
    ## (MAXGENS) has not been attained
    while ((currgen>0) && (numgen<MAXGENS))
      {
        numgen = numgen + 1 # increase the number of generations
        newgen = 0 # start new generation off at 0
        
        for (i in 1:currgen)
          newgen = newgen + sample(c(0,1,2), 1,replace=FALSE, prob=c(1/4, 1/4, 1/2))

        currgen = newgen # this becomes the current generation
      }
    if (newgen==0)
      extinction[j]=1 # it went extinct
    processLen[j]=numgen
    cat("done with branching process",i,"which had length",numgen,"\n")
  }

## count number of times the process reached (the artificially imposed) maximum length of MAXGENS
sum(processLen==MAXGENS)
## plot a histogram of the lengths before extinction (or before reaching MAXGENS)
hist(processLen, main="Lengths of branching process realizations")
