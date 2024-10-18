####################################
## Read in Titanic data example
## (See R-Bloggers) https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
####################################

### pre-processing
training.data.raw = read.csv("http://sites.stat.psu.edu/~mharan/380/data/TitanicTrain.csv",header=T,na.strings=c(""))
dataTitanic = subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))
dataTitanic$Age[is.na(dataTitanic$Age)] = mean(dataTitanic$Age,na.rm=T)
dataTitanic = dataTitanic[!is.na(dataTitanic$Embarked),]
rownames(dataTitanic) = NULL

### split into training and testing data
train = dataTitanic[1:800,]
test = dataTitanic[801:889,]

### BEGIN: predict survival based on Fare (how much they paid)
modelFit1 = glm(Survived ~ Fare,family=binomial(link='logit'),data=train)
summary(modelFit1)

## Cross-validation on test data
## By setting the parameter type='response', R will output probabilities
## in the form of P(y=1|X). 
fitted.results = predict(modelFit1,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
## Our decision boundary will be 0.5. If
## P(y=1|X) > 0.5 then y = 1 otherwise y=0.
fitted.results = ifelse(fitted.results > 0.5,1,0)
### Now look at proportion of misclassifications
misClasificError = mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))
### END: predict survival based on Fare (how much they paid)

### plot the data
plotcol= rep("black", length(train$Fare))
plotcol[train$Survived==1]="blue"
plotcol[train$Survived==0]="red"
plot(train$Fare , train$Survived, col=plotcol, main="Survival versus Fare")
plot(train$Age , train$Survived, col=plotcol, main="Survival versus Age")
table(train$Pclass, train$Survived)

### predict survival based on Age
modelFit2 = glm(Survived ~Age,family=binomial(link='logit'),data=train)
summary(modelFit2)
fitted.results = predict(modelFit2,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
## Our decision boundary will be 0.5. If
## P(y=1|X) > 0.5 then y = 1 otherwise y=0.
fitted.results = ifelse(fitted.results > 0.5,1,0)
### Now look at proportion of misclassifications
misClasificError = mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

### predict survival based on Class
modelFit3 = glm(Survived ~Pclass,family=binomial(link='logit'),data=train)
summary(modelFit3)
fitted.results = predict(modelFit3,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
## Our decision boundary will be 0.5. If
## P(y=1|X) > 0.5 then y = 1 otherwise y=0.
fitted.results = ifelse(fitted.results > 0.5,1,0)
### Now look at proportion of misclassifications
misClasificError = mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))


####################################
########## CART ############
####################################

cartFit = rpart(Survived ~ Pclass, data=train,method="class")

## 1) root 800 307 0 (0.6162500 0.3837500)  
##   2) Pclass>=2.5 439 108 0 (0.7539863 0.2460137) *
##   3) Pclass< 2.5 361 162 1 (0.4487535 0.5512465)  
##     6) Pclass>=1.5 167  80 0 (0.5209581 0.4790419) *
##     7) Pclass< 1.5 194  75 1 (0.3865979 0.6134021) *

## pdf("cartPlot.pdf")
## plot(cartFit)
## text(cartFit)
## dev.off()

## pdf("cartCrossVal.pdf")
## plotcp(cartFit)
## dev.off()

fit.preds = predict(cartFit,newdata=test,type="class")
fit.table = table(test$Survived,fit.preds)
fit.table
## > fit.table
##    fit.preds
##      0  1
##   0 51  5
##   1 18 15

cartFit2 = rpart(Survived ~ Pclass + Fare, data=train,method="class")
## print(cartFit2)
## n= 800 

## node), split, n, loss, yval, (yprob)
##       * denotes terminal node

##  1) root 800 307 0 (0.6162500 0.3837500)  
##    2) Pclass>=2.5 439 108 0 (0.7539863 0.2460137)  
##      4) Fare< 10.7979 293  61 0 (0.7918089 0.2081911) *
##      5) Fare>=10.7979 146  47 0 (0.6780822 0.3219178)  
##       10) Fare>=13.90835 137  38 0 (0.7226277 0.2773723) *
##       11) Fare< 13.90835 9   0 1 (0.0000000 1.0000000) *
##    3) Pclass< 2.5 361 162 1 (0.4487535 0.5512465)  
##      6) Fare< 52.2771 236 110 0 (0.5338983 0.4661017)  
##       12) Fare< 5.25 8   0 0 (1.0000000 0.0000000) *
##       13) Fare>=5.25 228 110 0 (0.5175439 0.4824561)  
##         26) Fare< 13.64585 74  29 0 (0.6081081 0.3918919) *
##         27) Fare>=13.64585 154  73 1 (0.4740260 0.5259740)  
##           54) Pclass< 1.5 74  32 0 (0.5675676 0.4324324) *
##           55) Pclass>=1.5 80  31 1 (0.3875000 0.6125000) *
##      7) Fare>=52.2771 125  36 1 (0.2880000 0.7120000) *
## pdf("cartPlot2.pdf")
## plot(cartFit2)
## text(cartFit2)
## dev.off()

## pdf("cartCrossVal2.pdf")
## plotcp(cartFit2)
## dev.off()

fit.preds2 = predict(cartFit2,newdata=test,type="class")
fit.table2 = table(test$Survived,fit.preds2)
fit.table2
  ## fit.preds2
  ##    0  1
  ## 0 53  3
  ## 1 17 16


##################################################
### fit using all predictors
##################################################
modelFit = glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(modelFit)

## By setting the parameter type='response', R will output probabilities
## in the form of P(y=1|X). Our decision boundary will be 0.5. If
## P(y=1|X) > 0.5 then y = 1 otherwise y=0.

fitted.results = predict(modelFit,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)

misClasificError = mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))
