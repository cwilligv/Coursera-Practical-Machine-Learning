library("ggplot2")
library("caret")

library("randomForest")
library("e1071")
library("gbm")
library("doParallel")
library("survival")
library("splines")
library("plyr")
library("RCurl")

library(rpart) # Regressive Partitioning and Regression trees
library(rpart.plot) # Decision Tree plot

# Setting the seed
set.seed(321)

#Loading training data from website provided.
trainingURL <- getURL('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

train <- read.csv(textConnection(trainingURL), header=T, na.strings = c("NA","#DIV/0!",""))

#Loading testing data from website provided.
testURL <- getURL('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

test <- read.csv(textConnection(testURL), header=T, na.strings = c("NA","#DIV/0!",""))

# Check dimensions for number of variables and number of observations
dim(train)
dim(test)

# Delete columns with all missing values
train<-train[,colSums(is.na(train)) == 0]
test <-test[,colSums(is.na(test)) == 0]

# deleting non meaningful variables.
train  <-train[,-c(1:7)]
test <-test[,-c(1:7)]

# new datasets:
dim(train)
dim(test)
#head(train)
#head(test)

# Partitioning data to allow cross validation.
ssample <- createDataPartition(y=train$classe, p=0.75, list=FALSE)
sTrain <- train[ssample, ] 
sTest <- train[-ssample, ]
dim(sTrain)
dim(sTest)
head(sTrain)
head(sTest)

plot(sTrain$classe, col="green", main="Levels of the variable classe within the sTrain data set", xlab="classe levels", ylab="Frequency")

# First prediction model
model_one <- rpart(classe ~ ., data=sTrain, method="class")

# Executing the prediction on the test data
prediction_one <- predict(model_one, sTest, type = "class")

# Let's plot the decision tree
rpart.plot(model_one, main="Classification Tree", extra=102, under=TRUE, faclen=0)

# Testing results on sTest data set:
print(confusionMatrix(prediction_one, sTest$classe))

# Second prediction model
model_two <- randomForest(classe ~. , data=sTrain, method="class")

# Predicting:
prediction_two <- predict(model_two, sTest, type = "class")

# Test results on subTesting data set:
print(confusionMatrix(prediction_two, sTest$classe))

# prediction of output levels from the original Testing data set using Random Forest algorithm
predict_test <- predict(model_two, test, type="class")
predict_test

# Writing files as requested in submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predict_test)