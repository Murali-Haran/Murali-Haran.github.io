
calcRainSize = function (x, traceAmt = 0) {
  mean(x[x> traceAmt])
}

args(median)
##function (x, na.rm = FALSE) 

## read in rain data again
load(url("http://www.stat.berkeley.edu/users/nolan/data/rainfallCO.rda"))

## define function
calcRainSize = function(x) mean(x[x>0])

sapply(rain, calcRainSize)

sapply(rain, function(station) { mean(station[station > 0])})

sapply(rain, calcRainSize, 
       traceAmt = 5)
mapply(calcRainSize, rain, 
       traceAmt = c(0, 1, 5, 10, 0))          

calcRainSize = 
 function(x, tA = 0, sumFun = mean) 
{
  sumFun(x[x > tA])
}

mapply(calcRainSize, rain, 
       traceAmt = c(0, 1, 5, 10, 0),
       MoreArgs = list(sumFun = median))

############################
## Roulette example
############################

wheel = rep(c("red","black","green"),c(18, 18, 2))

spins = sample(wheel, 100, 
               replace = TRUE)

winnings = numeric(100)
winnings[spins == "red"] = 1
winnings[spins != "red"] = -1

totalWinnings = sum(winnings)

wheel = rep(c(1, -1), c(18, 20))

wins = sample(wheel, 100, 
               replace = TRUE)

totalWinnings = sum(wins)

### another scenario
wheel = rep(c(1, -1), c(18, 20))

wins = sample(wheel, 1, 
              replace = TRUE)

totalWinnings = sum(wins * 100)

### encapsulate
betRed = 
  function(numBets,betAmt = 1) {
  
  wheel = rep(c(1, -1), c(20, 18))
  wins = sample(wheel, numBets,
                replace = TRUE)

  totWinnings = sum(wins * betAmt)
  return(totWinnings)
}

> betRed(1, 100)
[1] -100
> betRed(1, 100)
[1] 100
> betRed(1, 100)
[1] 100
> betRed(1, 100)
[1] -100
> betRed(1, 100)
[1] -100
> betRed(1, 100)
[1] -100
> betRed(1, 100)
[1] -100
> betRed(100, 1)
[1] 8
> betRed(100, 1)
[1] -8
> betRed(100, 1)
[1] 10
> betRed(100, 1)
[1] -8
> betRed(100, 1)
[1] -20
> betRed(100, 1)
[1] -2
> betRed(100, 1)
[1] -4

## Call function many times
red100B.1D = sapply(rep(100, 10000),
               betRed, betAmt = 1)

red1B.100D = sapply(rep(1, 10000),
               betRed, betAmt = 100)

## Notice that the calls to betRed do not depend on the input – shortcut

replicate(10000, betRed(100, 1))
