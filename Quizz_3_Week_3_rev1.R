# Machine Learning
# Quizz 3 - Semana 3

# Questão 1

library(dplyr)
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)
library(partykit)
library(rattle)

r_train <- segmentationOriginal$Case == "Train"
r_test <- segmentationOriginal$Case == "Test"

# Drop the columns of the dataframe
training <- as.data.frame(select(segmentationOriginal[r_train, ],-c(Case)))
testing <- select(segmentationOriginal[r_test, ],-c(Case))

set.seed(125)
modFit <- train(Class ~ . , method = "rpart", data = training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform = TRUE)
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
fancyRpartPlot(modFit$finalModel)

modFit_a <- as.party(modFit)

TotalIntenCh2<- c(23000,50000,57000,NA)
FiberWidthCh1<- c(10,10,8,8)
VarIntenCh4<- c(NA,100,100,100)
PerimStatusCh1<- c(2,NA,NA,2)

theTestData <- data.frame(TotalIntenCh2,
                          FiberWidthCh1,
                          VarIntenCh4,
                          PerimStatusCh1)


# Por eliminação a resposta é PS, WS, PS, Impossível predizer



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
data(vowel.train)
data(vowel.test)

set.seed(33833)





# Para a Questão 5

# I got to the correct answer after I sorted by "MeanDecreaseGini" in 
# modFit$importance after fitting with randomForest() - when I ran it the 
# second time (first time got a different answer!)

