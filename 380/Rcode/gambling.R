################################################
## Simulate a very simple game: a coin toss
################################################

## a single coin toss
flipcoin=function(side)
    {
        coin=sample(c("H","T"), 1)
        return(as.integer(side==coin))
    }

## repeat this 10,000 times
## using a loop
M=10000
wins=rep(NA, M)
for (i in 1:M)
    wins[i]=flipcoin("H")
mean(wins) ## this is the average of the winnings

## slicker way to do above (without a loop)
flipcoin10 = replicate(10, flipcoin("H")) # repeat 10 times
flipcoin10000 = replicate(10000, flipcoin("H")) # repeat 10,000 times

################################################
## Simulate Chuck-a-Luck gambling game
## played with 3 dice
## Gambler selects an integer from 1 to 6
## Three dice are rolled
## If exactly k dice show the gambler's number,
## the payoff is k:1
################################################

## This function plays a single game of Chuck-a-Luck
## Input: gambler's choice of number (num)
## Output: payoff
chuckaluck=function(num)
    {
        dierolls=sample(seq(1,6), 3, replace=TRUE) ## roll 3 dice
        payoff=sum(dierolls==num) ## count the number of dice = gamble value
        return(payoff)
    }

## Is this a fair game? That is, are the odds 1:1?

## Repeat this many times to find expected wins
## Not necessarily most efficient but easiest to follow
M = 100000 # number of repititions
payoffvec=rep(NA, M)
for (i in 1:M)
    payoffvec[i] = chuckaluck(6)
mean(payoffvec)

## Shorter code for above
chuckaluck10000 = replicate(10000, chuckaluck(2)) # repeat 10,000 times
## Notice that this code is faster! You will often find that
## avoiding for loops can speed things up though this is not always guaranteed
## for complicated problems, it can be important to find ways to make your
## code more efficient

################################################
## 


################################################
## Another gambling game: High-low
## roll a pair of fair dice.
## high if the sum is 8, 9, 10, 11, or 12.
## low if the sum is 2, 3, 4, 5, or 6
## seven if the sum is 7
## A player can bet on any of the three outcomes.
## The payoff for a bet of high or for a bet of low is 
## 1:1. The payoff for a bet of seven is is 4:1
## input: the bet, either "hi", "low" or "7" (default="7")
################################################

highlow=function(x="7")
  {
    if (!(x %in% c("high","low","7")))
      stop("x must be a string, one of: high low or 7")
    
    dierolls=sample(seq(1,6), 2, replace=TRUE) ## roll 2 dice
    dietotal=sum(dierolls)
    if (x=="7")
      if (dietotal==7) {payoff=4+1} else {payoff=0}
    else
      if (x=="high")
        if (dietotal>7) {payoff=1+1} else {payoff=0}
      else if (x=="low")
        if (dietotal<7) {payoff=1+1} else {payoff=0}

    return(payoff)
  }
