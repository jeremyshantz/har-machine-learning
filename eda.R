suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(randomForest))

rm(list=ls())
# Load the training and testing data sets; 
rawData <- read.csv('./data/pml-training.csv')
finalTestData <- read.csv('./data/pml-testing.csv')

# Partition the training data into training, testing, validation data sets
# 60% of the data is used to train our model
# 40% of the data is reserved to test against so we can measuure 
#   the effectiveness of our model
# The test data is divided equally between testing and validation sets
indexes <- createDataPartition(y=rawData$classe, p=0.60, list=FALSE)
train.original <- rawData[indexes,]
testset <- rawData[-indexes,]
test.indexes <- createDataPartition(y=testset$classe, p=0.50, list=FALSE)
test <- testset[test.indexes,] 
validate <- testset[-test.indexes,] 

## Feature Selection
###Remove the first 7 columns which are clearly not predictors
train <- train.original[, c(-7:-1)]

#### Remove columns with little variation
nzvar <- nearZeroVar(train, saveMetrics=TRUE)
train <- train[, !(nzvar$nzv)]

# even now there are still a lof of NA
head(train[, sample(names(train), 8)])

# if the rows without NAs all has the same classes or same two classes, they might be intersting features; 
# instead, all classes are represented; and its suspicious that all the NA columsn have the same sum;
# remove all non-zero columns
sapply(1:ncol(train) - 1, function(i) { sum(is.na(train[,i]))  })
indexes <- sapply(1:98, function(i) { ifelse(sum(is.na(train[,i])) > 10000, -i, i)  })
train <- train[, indexes[indexes < 0]]

# fit a model
model <- train(classe ~., data=train, method="rf", tuneGrid=data.frame(mtry=2) )

predictions <- predict(model, train)
mt <- confusionMatrix(predictions, train$classe)

mt

confusionMatrix(predict(model, train), train$classe)
confusionMatrix(predict(model, test), test$classe)
confusionMatrix(predict(model, rawData), rawData$classe)

answers <- as.character(predict(model, finalTestData))

# confirmation
varibleImp <- varImp(model)
vars <- varibleImp[1][[1]]
vars$sensor <- row.names(vars)
vars <- vars[order(vars$Overall, decreasing=TRUE),]
vars

## random sub-sampling cross-validation


## Future Studies
only the dumbbell sensor, the Kinect data
http://groupware.les.inf.puc-rio.br/public/papers/2013.Velloso.QAR-WLE.pdf


## k-fold cross-validation
folds <-createFolds(y=train$classe, k=10, list=TRUE, returnTrain=TRUE)
sapply(folds, length)
folds[[1]]
folds[[1]][1:10]

plot(roll_belt ~ classe, data=raw)


## leave one out

# pick prediction function using cross-validation
# use this comment: since we cross-validate to pick predictors estimate we must estimate errors on independent data. 
# out validate set was reserved for this purpose; Because it is independant of the data used to train out model.
# the error estiate it yields is therefore reliable.
# 

# apply to test set and refine


# apply one time to validation

