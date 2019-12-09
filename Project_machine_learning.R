

library(dplyr)
library(taRifx)

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

check_valid <- as.data.frame(apply(raw_training, 2, count_na))

columns_with_NA <- check_valid[,1]==19216
features_names <- names(raw_training)
features_useful <- features_names[!columns_with_NA]
training <- raw_training[,features_useful]
head(training)
