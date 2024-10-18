### congruential random number generator

cong = function(n,a,b,seed)
    {
        randVals=rep(NA,n)
        randVals[1]=seed
        for (i in 2:n)
            randVals[i]=a*randVals[i-1] %% b
        return(randVals)
    }
