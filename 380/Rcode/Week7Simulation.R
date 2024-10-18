### Simulating medians of normal r.v.s
###Generate n random normal values from a Normal(0, s^2)
###Take the median of these n values
### A single experiment
n = 27
s = 3
median(rnorm(n = n, sd = s))

### Multiple experiments
B = 1000
sampleMs = replicate(1000, median(rnorm(n = n, sd = s))
mean(sampleMs)
sd(sampleMs)
hist(sampleMs)

### Try different versions of above
ns = seq(20, 200, by = 10)
ss = seq(1, 10, by = 0.5)

#### Repeat above simulation
ns = seq(20, 200, by = 10)
ss = seq(1, 10, by = 0.5)
samples = matrix(nrow = length(ns),ncol = length(ss))

for (i in 1:length(ns)) {
  for (j in 1:length(ss)) {
   samples[i , j] =  
    mean(replicate(1000, 
           median(rnorm(n = ns[i], 
                        sd = ss[j]))))
  }
}

### Another way to do this
### First: example of expand.grid
### take all combinations of provided vectors
    expand.grid(1:3, c("a","b","c","d"))
    
    ns = seq(20, 200, by = 10)
    ss = seq(1, 10, by = 0.5)
    ### Create a matrix to hold samples
    samples = matrix(nrow = length(ns), ncol = length(ss))
    parValues = expand.grid(ns, ss)

### apply the anonymous function to each row of matrix
### (parValues first column, parValues second column)
    simResults = mapply(function(num, sd)
        { 
            mean(replicate(1000,median(rnorm(num,sd))))
        },  
        parValues[ , 1], parValues[, 2])
    
