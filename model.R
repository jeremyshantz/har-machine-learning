# exploratory work prior to writing the report

rm(list=ls())
setwd('~/courses/practical_machine_learning/project/')
set.seed(32343)

suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(randomForest))
# load the train data set; 
rawData <- read.csv('./data/pml-training.csv')
finalTestData <- read.csv('./data/pml-testing.csv')
dim(raw) # 19622 rows, 160 columns

# Partition data into training, testing, validation data sets
# 60% of the data is used to train our model
# 40% of the data is reserved to test against so we can measuure 
#   the effectiveness of our model
# The test data is divided equally between testing and validation sets
indexes <- createDataPartition(y=rawData$classe, p=0.60, list=FALSE)
train <- rawData[indexes,]
testset <- rawData[-indexes,]
test.indexes <- createDataPartition(y=testset$classe, p=0.50, list=FALSE)
test <- testset[test.indexes,] 
validate <- testset[-test.indexes,] 
dim(train)
dim(test)
dim(validate)
View(validate)
# Removing columns
#Remove the first 7 columns which are clearly not predictors
smaller <- train[, c(-1,-2,-3,-4,-5,-6,-7)]
# Picking features v- what data should we use?

# Remove columns with little variation
nsv <- nearZeroVar(smaller, saveMetrics=TRUE)
nsv
smaller <- smaller[, !(nsv$nzv)]
dim(smaller)

# even now there are still a lof of NA
# if the rows without NAs all has the same classes or same two classes, they might be intersting features; 
# instead, all classes are represented; and its suspicious that all the NA columsn have the same sum;
sapply(1:98, function(i) { sum(is.na(smaller[,i]))  })
indexes <- sapply(1:98, function(i) { ifelse(sum(is.na(smaller[,i])) > 10000, -i, i)  })
smaller <- smaller[, indexes[indexes < 0]]
View(smaller)

# fit a model
dim(smaller)
modelFit <- train(classe ~., data=smaller, method="rf", tuneGrid=data.frame(mtry=2) )
modelLookup('glm')

modelFit$finalModel
predictions <- predict(modelFit, train)
confusionMatrix(predictions, smaller$classe)
confusionMatrix(predict(modelFit, test), test$classe)
confusionMatrix(predict(modelFit, raw), raw$classe)

answers <- as.character(predict(modelFit, finalTestData))

## random sub-sampling cross-validation

## k-fold cross-validation
folds <-createFolds(y=train$classe, k=10, list=TRUE, returnTrain=TRUE)
sapply(folds, length)
folds[[1]]
folds[[1]][1:10]


## leave one out

# pick prediction function using cross-validation
# use this comment: since we cross-validate to pick predictors estimate we must estimate errors on independent data. 
    # out validate set was reserved for this purpose; Because it is independant of the data used to train out model.
    # the error estiate it yields is therefore reliable.
# 

# apply to test set and refine


# apply one time to validation

# exploratory
modelFit <- train(classe ~., data=train, method="rf")
modelFit
modelFit$finalModel

predictions <- predict(modelFit,newdata=testing)
predictions

confusionMatrix(predictions,testing$type)

# exploratory plotting
featurePlot(x=train[, c('gyros_forearm_x', 'magnet_forearm_y', 'var_pitch_forearm')],
            y = train$classe, plot='pairs')

qplot(var_pitch_forearm, classe, data=train)

# http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises
# belt, forearm, arm, dumbell  
grep('bell', names(train), value=TRUE)
