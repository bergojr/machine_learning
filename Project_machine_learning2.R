

library(dplyr)
library(taRifx)
library(skimr)
library(caret)
library(rpart)
library(parallel)
library(doParallel)

URL_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
URL_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

fileNameTraining <- "pml-training.csv"
fileNameTesting <- "pml-testing.csv"


# Download files if not available

if(!file.exists(fileNameTraining)){
  download.file(URL_training,fileNameTraining) 
}

if(!file.exists(fileNameTesting)){
  download.file(URL_testing,fileNameTesting) 
}

count_na <- function(x) {
  if (any(is.na(x))) {
    return(sum(is.na(x)))
  } else {
    return (sum(x=="NA"))
  }
}


raw_training <- read.csv2("pml-training.csv",header = TRUE, sep = ",", 
                          na.strings = c("","<NA>"))
raw_testing <- read.csv2("pml-testing.csv",header = TRUE, sep = ",", 
                          na.strings = c("","<NA>"))

# skimmed <- skim(raw_training)
# skimmed[, c(1:5, 9:11, 13, 15:16)]


check_valid <- as.data.frame(apply(raw_training, 2, count_na))

columns_with_NA <- check_valid[,1]==19216
features_names <- names(raw_training)
features_useful <- features_names[!columns_with_NA]
training_full <- raw_training[,features_useful]
#head(training)
testing_full <- raw_testing[,features_useful[1:59]]

training <- training_full[,8:60]
testing <- testing_full[,8:59]


col_trt <- dim(training)[2]
col_tst <- dim(testing)[2]

training[, 1:(col_trt-1)] <- remove.factors(training[,1:(col_trt-1)])
testing <- remove.factors(testing)

training[1:52] <- as.numeric(unlist(training[1:52]))
testing[1:52] <- as.numeric(unlist(testing[]))


rm(testing_full)
rm(training_full)
rm(raw_testing)
rm(raw_training)

set.seed(1921)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(training$classe, p=0.7, list=FALSE)

# Step 2: Create the training  dataset
trainData <- training[trainRowNumbers,]

# Step 3: Create the test dataset
validData <- training[-trainRowNumbers,]


# define training control
train_control <- trainControl(method="cv", number=5, allowParallel = TRUE)
# train the model

if (!file.exists("final_model_1112.rds")){
  # Initializing cluster
  cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
  registerDoParallel(cluster)
  
  # Training model
  model <- train(classe ~., data=trainData, trControl=train_control, method="rf")
  
  # Stopping clusters
  stopCluster(cluster); registerDoSEQ(); rm(list = "cluster")
  # Saving model 
  saveRDS(model, file = "final_model_1112.rds")
} else {
  # Restore model
  model <- readRDS(file = "final_model_1012.rds")
}


# summarize resultsmemory.limit()

print(model)


pred_valid <- predict(model, validData)



conf_matrix <- confusionMatrix(pred_valid,validData$classe)
conf_matrix
conf_matrix$table
round(conf_matrix$overall,4)


## Dados para responder ao Quizz 4 - Week 4

pred_quizz <- predict(model, testing)
# B A B A A E D B A A B C B A E E A B B B - Iteração em 22:17
# B A B A A E D B A A B C B A E E A B B B - Iteração em 21:39
# B A B A A E D B A A B C B A E E A B B B - iteração em 21:34
# B A A A A E D B A A B C B A E E A B B B