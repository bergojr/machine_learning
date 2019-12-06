# Machine Learning
# Quizz 3 - Semana 3

# Questão 1


library(ElemStatLearn)
library(dplyr)
library(caret)
library(rpart)
library(partykit)
library(rattle)
data(vowel.train)
data(vowel.test)

training <- vowel.train
testing <- vowel.test

training$y <- as.factor(training$y)
testing$y <- as.factor(testing$y)

set.seed(33833)

mod1 <- train(y ~., data = training , method="rf")
mod2 <- train(y ~., data = training , method="gbm")
pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)

# Overall accuracy for random forest
confusionMatrix(pred1,testing$y)$overall[1]

# Overall accuracy for random gbm
confusionMatrix(pred2, testing$y)$overall[1]

# Aggrement accuracy

eq_predict <- pred1==pred2

comm_val <- as.data.frame(cbind(pred1[eq_predict],
                                pred1[eq_predict],
                                testing$y[eq_predict]))

agree_accuracy <- sum(comm_val$V1==comm_val$V3)/length(comm_val$V1)
agree_accuracy2 <- sum(comm_val$V1==comm_val$V3)/length(pred1)

# Questão 2

library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# rf, gbm, lda

mod_rf <- train(diagnosis ~ ., data = training, method = "rf")
mod_gbm <- train(diagnosis ~ ., data = training, method = "gbm", verbose = FALSE)
mod_lda <- train(diagnosis ~ ., data = training, method = "lda")

pred_rf <- predict(mod_rf,testing)
pred_gbm <- predict(mod_gbm,testing)
pred_lda <- predict(mod_lda,testing)

confusionMatrix(pred_rf,testing$diagnosis)$overall[1]
confusionMatrix(pred_gbm,testing$diagnosis)$overall[1]
confusionMatrix(pred_lda,testing$diagnosis)$overall[1]

stacked_predicitions <- data.frame(pred_rf,
                                   pred_gbm,
                                   pred_lda,
                                   diagnosis = testing$diagnosis)

mod_stacked <- train(diagnosis ~ .,
                     data = stacked_predicitions, 
                     method = "rf")

pred_stacked <- predict(mod_stacked,stacked_predicitions)
confusionMatrix(pred_stacked,testing$diagnosis)$overall[1]

# Questão 3

library(pgmm)
data(olive)
olive = olive[,-1]

oliveFit <- train(Area ~. , method = "rpart", data = olive)



newdata = as.data.frame(t(colMeans(olive)))

predArea <- predict(oliveFit,newdata = newdata)


fancyRpartPlot(oliveFit$finalModel)

# Questão 4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

# chdFit <- train(I(factor(chd)) ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm",
#                 family="binomial", data = trainSA)


chdFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm",
                family="binomial", data = trainSA)


predTrainSA <- predict(chdFit, newdata = trainSA)
predTestSA <- predict(chdFit, newdata = testSA)


missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, as.numeric(predTrainSA))
missClass(testSA$chd, as.numeric(predTestSA))


# Questão 5

library(ElemStatLearn)
library(randomForest)
library(dplyr)

data(vowel.train)
data(vowel.test)

set.seed(33833)

rf1 <- randomForest(x = select(vowel.train,-c(y)), y=vowel.train$y,  
             xtest=select(vowel.test,-c(y)), ytest=vowel.test$y,
             importance = TRUE)



# Para a Questão 5

# I got to the correct answer after I sorted by "MeanDecreaseGini" in 
# modFit$importance after fitting with randomForest() - when I ran it the 
# second time (first time got a different answer!)

