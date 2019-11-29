# Question 1

library(caret)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
library(AppliedPredictiveModeling)

# opção 1
adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
dim(training)
dim(testing)


# opção 2
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


# Questão 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(Hmisc)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
plot(inTrain, training$CompressiveStrength, col=training$FlyAsh)
