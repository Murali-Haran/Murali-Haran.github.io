load(url("http://www.stat.berkeley.edu/users/nolan/data/anExampleList.rda"))

str(aList) ### description of structure

aList["listToo"]
aList[ c(3, 2)] 
aList[ -(1:3) ]
aList$listToo
aList[[2]]

aList$listToo$aVec
aList[[1]]$aVec
aList[[1]][[1]]
aList$listToo[[1]]

aList$aVec[1:3]
aList[[4]]$id
aList[[4]][1:2, 2:3]

aList[[3]](2:3)

load(url("http://www.stat.berkeley.edu/users/nolan/data/rainfallCO.rda"))

class(rain)
length(rain)
names(rain)

str(rain)

class(rain$st050183)
length(rain$st050183)
head(rain$st050183)

class(rain[["st050945"]])
length(rain[["st050945"]])
head(rain[[5]])

lapply(rain, mean)
rain$st050183
rain$st050263
rain$st050712
rain$st050843
rain$st050945

sapply(rain, mean)

lapply(rain, mean, na.rm = TRUE,trim = 0.1)
rain$st050183
rain$st050263
rain$st050712
rain$st050843
rain$st050945

args(lapply)

sapply(rain, max)
##99th percentile of rainfall at each station
sapply(rain, quantile, probs = 0.99)

## for one station
stat1 = rain[[1]]
sum(stat1 > 0) / length(stat1)

numDays = sapply(rain, length) 
rainDays = lapply(rain, '>', 0)
numRainyDays = sapply(rainDays, sum)
numRainyDays / numDays

sum(stat1 > 0) / length(stat1)
##We want to apply this composition of functions to each element of rain. 
sapply(rain, sum(? > 0) / length(?)) ## problem with this 
##We can create our own function to do this:
sapply(rain, function(x) sum(x > 0) / length(x))

### Matrices
m = matrix(1:6, nrow = 2)
m
##      [,1] [,2] [,3]
## [1,]    1    3    5
## [2,]    2    4    6
m = matrix(1:6, nrow = 2, byrow = TRUE)
m

rownames(m) = letters[1:2]
colnames(m) = letters[3:5]
m
dim(m)
nrow(m)
ncol(m)

m[-1, 2]    # Exclusion & position
m["a",]     # Row by name, all cols 
m[, c(TRUE, TRUE, FALSE)] #logical cols
m
apply(m, 1, sum)
apply(m, 2, sum)

x = array(1:24, c(4, 3, 2))
x

x[1:2, 3, 2]
##[1] 21 22

x[ , 2, 1]
##[1] 5 6 7 8

x[3:4, c(3, 1), 1]

x = array(1:24, c(4, 3, 2))
x
apply(x, 1:2, sum)
apply(x, 1, median)


