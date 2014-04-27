#============================
#Using GLMS to Classifiy Wine
#============================

#=========
#Libraries
#=========

library(stats)#For backward model
library(boot)#For cross-validation
library(ggplot2)#For plots

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

summary(train[c('fixed.acidity')])
m <- ggplot(train, aes(x=fixed.acidity))
m + geom_histogram(aes(fill = ..count..),binwidth=.5) + ggtitle("Fixed Acidity") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('fixed.acidity')])
m <- ggplot(train, aes(x=fixed.acidity))
m + geom_histogram(aes(fill = ..count..),binwidth=.5) + ggtitle("Fixed Acidity") + theme(plot.title = element_text(lineheight=.8, face="bold"))

summary(train[c('fixed.acidity')])
m <- ggplot(train, aes(x=fixed.acidity))
m + geom_histogram(aes(fill = ..count..),binwidth=.5) + ggtitle("Fixed Acidity") + theme(plot.title = element_text(lineheight=.8, face="bold"))


#=======================================
#First Model: Simple Logistic Regression
#=======================================

logistic = glm(good~.,family=binomial, data=train)
summary(logistic)

#==================================
#Second Model: Backward Elimination
#==================================

back = step(logistic,direction="backward")
summary(back)
