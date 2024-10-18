########################################
## read in data set
## The data (X1, X2, X3, X4, X5) are by city.
## X1 = death rate per 1000 residents
## X2 = doctor availability per 100,000 residents
## X3 = hospital availability per 100,000 residents
## X4 = annual per capita income in thousands of dollars
## X5 = population density people per square mile
## Reference: Life In America's Small Cities, by G.S. Thomas
########################################
health=read.csv("http://personal.psu.edu/muh10/380/data/mlr07.csv")
## response: 
names(health) = c("deathRate","drAvail","hospAvail","income","popDensity")

## fit linear regression
##attach(health)
fitLM=lm(deathRate~drAvail+hospAvail+income+popDensity,data=health)
##sum(fitLM$residuals^2)

## Now do cross-validation "by hand" (without using a package)
## first, try fitting 40 data points and testing on remaining 13
trainDat=health[1:40,]
testDat=health[41:53,]

fitLM=lm(deathRate~drAvail+hospAvail+income+popDensity,data=trainDat)
predLM=predict(fitLM, testDat)
sum((testDat$deathRate-predLM)^2) # squared differences

### how do we repeat the above many times?
### STEP 1: write a function to do this once
### STEP 2: replicate it many times and look at the distribution
### of results (squared differences in this case)
### How do we do this....

########################################
## another example data set
## https://archive.ics.uci.edu/ml/datasets.html?task=reg
## Beijing PM2.5 data
########################################
pm25=read.table("http://personal.psu.edu/muh10/380/data/PRSA_data_2010.1.1-2014.12.31.csv",sep=",",header=TRUE)
fitPM25=lm(pm2.5~DEWP+TEMP+PRES,data=pm25)


########################################
## Cross-validation for logistic regression
########################################
## example data set
## location: https://archive.ics.uci.edu/ml/datasets/Parkinsons",
parkinsons=read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data", sep=",", header=TRUE)

## basic exploration of the data set
dim(parkinsons)
names(parkinsons)
sum(parkinsons$status)/length(parkinson$status) # tells you proportion with Parkinsons
fitLogistic =glm(status ~ MDVP.Fo.Hz.+MDVP.Fhi.Hz.+MDVP.Flo.Hz.,family=binomial(link='logit'),data=parkinsons)
summary(fitLogistic)







## Let us use cross-validation to see how well this works
## with test (hold-out) data
#### train about 90% (175) and test about 10% (20)
#### OR: train on about 50% (100), test on about 50% (95)
trainInd=sample(seq(1,195), 100, replace=FALSE)
trainDat=parkinsons[trainInd,]
testDat=parkinsons[-trainInd,]
fitLogistic =glm(status ~ MDVP.Fo.Hz.+MDVP.Fhi.Hz.+MDVP.Flo.Hz.,family=binomial(link='logit'),data=trainDat)
summary(fitLogistic)
predLogistic=predict(fitLogistic, testDat,type="response") ## type=response is there to ensure predictions are in (0,1)
binPred=ifelse(predLogistic > 0.5,1,0) # shorthand for converting >0.5 to 1 and <=0.5 to 0
sum(binPred==testDat$status)/length(testDat$status) ## to find the proportion of misclassifications
## [1] 0.8
## to find the proportion of misclassified 1's
ind1=(testDat$status==1)
sum(binPred[ind1]==testDat$status[ind1])/length(testDat$status[ind1]) ## to find the proportion of misclassifications
## to find the proportion of misclassified 0's
sum(binPred[-ind1]==testDat$status[-ind1])/length(testDat$status[-ind1]) ## to find the proportion of misclassifications

### now replicate this


##### Bootstrap
### recall that you can also use another sampling technique, the bootstrap
### to approximate standard errors of the parameters of the model


