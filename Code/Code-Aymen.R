#============================
#Using GLMS to Classifiy Wine
#============================

#=========
#Libraries
#=========

library(stats)#For backward model
library(boot)#For cross-validation
library(ggplot2)#For plots
library(arm)#For cook's distances + residuals

#====
#Data
#====

train = read.csv('train.csv',header=T)
test = read.csv('test.csv',header=T)

#==============
#Data Summaries
#==============

summary(train[c('fixed.acidity')])
m <- ggplot(train, aes(x=fixed.acidity))
m + geom_histogram(aes(fill = ..count..),binwidth=.1) + ggtitle("Fixed Acidity") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('volatile.acidity')])
m <- ggplot(train, aes(x=volatile.acidity))
m + geom_histogram(aes(fill = ..count..),binwidth=.01) + ggtitle("Volatile Acidity") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('citric.acid')])
m <- ggplot(train, aes(x=citric.acid))
m + geom_histogram(aes(fill = ..count..),binwidth=.01) + ggtitle("Citric Acid") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('residual.sugar')])
m <- ggplot(train, aes(x=residual.sugar))
m + geom_histogram(aes(fill = ..count..),binwidth=.2) + ggtitle("Residual Sugar") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('chlorides')])
m <- ggplot(train, aes(x=chlorides))
m + geom_histogram(aes(fill = ..count..),binwidth=.001) + ggtitle("Chlorides") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('density')])
m <- ggplot(train, aes(x=density))
m + geom_histogram(aes(fill = ..count..),binwidth=.0001) + ggtitle("Density") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('pH')])
m <- ggplot(train, aes(x=pH))
m + geom_histogram(aes(fill = ..count..),binwidth=.01) + ggtitle("pH") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('sulphates')])
m <- ggplot(train, aes(x=sulphates))
m + geom_histogram(aes(fill = ..count..),binwidth=.01) + ggtitle("Sulphates") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('alcohol')])
m <- ggplot(train, aes(x=alcohol))
m + geom_histogram(aes(fill = ..count..),binwidth=.1) + ggtitle("Alcohol") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('free.so2')])
m <- ggplot(train, aes(x=free.so2))
m + geom_histogram(aes(fill = ..count..),binwidth=.9) + ggtitle("Free SO2") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('total.so2')])
m <- ggplot(train, aes(x=total.so2))
m + geom_histogram(aes(fill = ..count..),binwidth=1) + ggtitle("Total SO2") + theme(plot.title = element_text(lineheight=.8, face="bold"))

#================
#Outlier Analysis
#================

order(train[,c('fixed.acidity')],decreasing=T)[c(1:10)] #619 1339 1077 3410 3091 3466   66 2469  561 1686
order(train[,c('volatile.acidity')],decreasing=T)[c(1:10)] #2322 2469  707 3483 2168 3056 1655   37 1747 2251
order(train[,c('citric.acid')],decreasing=T)[c(1:10)] #322 1139 2048  357 1036  301 2773 1668 3434  660
order(train[,c('residual.sugar')],decreasing=T)[c(1:10)] #247  904 3533 2076 1609  603  995 2984 2556  442
order(train[,c('chlorides')],decreasing=T)[c(1:10)] #2705 2174 2326 1912 3495 1805  497  334  363 1434
order(train[,c('density')],decreasing=T)[c(1:10)] #247  904 3533  682 2076  442 1194 2353 3476 1204
order(train[,c('pH')],decreasing=T)[c(1:10)] #432  435  651 1191 2726 3249 3287 1114 2134 2861
order(train[,c('sulphates')],decreasing=T)[c(1:10)] #2835  984 2294  888  406 1008 1409 1063 3002   72
order(train[,c('alcohol')],decreasing=T)[c(1:10)] #2320  839   99 1069 1917 3149 1396 2340 2455 2403
order(train[,c('free.so2')],decreasing=T)[c(1:10)] # 1690 3339 1650 2970 3436  682 1914 2496  767 1345
order(train[,c('total.so2')],decreasing=T)[c(1:10)] #1690  537 3671 2942 2725  486 3339   69  966 2970


#=======================================
#First Model: Simple Logistic Regression
#=======================================

formula1 = good~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+density+pH+sulphates+alcohol+free.so2+total.so2
logistic1 = glm(formula1,family=binomial, data=train)
summary(logistic1)

#CORRELATION

vif(logistic1) #remove density which seems to be correlated to residual.sugar

formula2 = good~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+pH+sulphates+alcohol+free.so2+total.so2
logistic2 = glm(formula2,family=binomial, data=train)
summary(logistic2)
vif(logistic2)#No correlation anymore

#ANOVA

anova(logistic2, logistic1,test="Chi")
#Even with correlation , the additive model is statistically significantly better than the one without density.

#RESIDUALS + COOK's DISTANCES

wine.fitted = fitted(logistic1) # fitted probabilities
wine.devresid = residuals(logistic1,type="deviance") # deviance residuals
wine.jresid = rstudent(logistic1) # approximate studentized/jackknifed residuals
wine.cooks = cooks.distance(logistic1) # approximate Cooks distances

##Fitted Values vs Deviance residuals
binnedplot(wine.fitted, wine.devresid,
           xlab="Averaged fitted probabilities",
           ylab="Averaged deviance residuals",
           pch=19, col.pts="red", cex.pts=0.5,
           main="Fitted Values vs deviance residual plot")
abline(h=0,lty=2,col="green")

##Fitted Values vs Jackknifed residuals
binnedplot(wine.fitted, wine.jresid,
           xlab="Averaged fitted probabilities",
           ylab="Averaged jackknifed residuals",
           pch=19, col.pts="red", cex.pts=0.5,
           main="Fitted Values vs jackknifed residual plot")
abline(h=0,lty=2,col="green")

# Cook's distances
plot(wine.cooks, type="h", lwd=2,xlab="Observation index",ylab="Cook's distances",main="Cook's distances")
abline(h=1,lty=2,col="red")

#================
#Cross-Validation
#================

logloss <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, rep(epsilon)), 1-rep(epsilon))
  ll <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat))
  return(ll)
}


val.10.fold <- cv.glm(data=train, glmfit=logistic2,cost=logloss, K=10)
val.10.fold$delta

#==========================
#Prediction on the test set
#==========================

predictions = predict(logistic1,test,type="response")
write.csv(predictions,"simple_logistic_regression_corrected.csv",row.names = T)


#==================================
#Second Model: Backward Elimination
#==================================

back = step(logistic,direction="backward")
summary(back)

formula3 = good~volatile.acidity+residual.sugar+pH+sulphates+alcohol+free.so2
logistic3 = glm(formula3,family=binomial, data=train)
summary(logistic3)

val.10.fold <- cv.glm(data=train, glmfit=logistic3,cost=logloss, K=10)
val.10.fold$delta


formula4 = good~fixed.acidity*volatile.acidity+citric.acid*residual.sugar+chlorides+pH*sulphates+alcohol*free.so2
logistic4 = glm(formula4,family=binomial, data=train)
summary(logistic4)

val.10.fold <- cv.glm(data=train, glmfit=logistic3,cost=logloss, K=10)
val.10.fold$delta
anova(logistic3,logistic2,logistic1,test="Chi")

#Removing the Sample with the max cook's distance
indicesToRemove = c(which.max(wine.cooks),619,2322,322,247,2705,435,2835,2320,1690)
train2 = train[-indicesToRemove,]
logistic5 = glm(formula1,family=binomial, data=train2)
val.10.fold <- cv.glm(data=train2, glmfit=logistic5,cost=logloss, K=10)
val.10.fold$delta
