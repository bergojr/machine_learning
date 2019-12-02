# Machine Learning
# Quizz 3 - Semana 3

# Questão 1

library(dplyr)
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)

# Drop the columns of the dataframe
select (mydata,-c(mpg,cyl,wt))

r_train <- segmentationOriginal$Case == "Train"
r_test <- segmentationOriginal$Case == "Test"

train <- as.data.frame(select(segmentationOriginal[r_train, ],-c(Case)))
test <- select(segmentationOriginal[r_test, ],-c(Case))

set.seed(125)

tree1 <- rpart(Class ~ ., data = train)

tree1a <- as.party(tree1)
