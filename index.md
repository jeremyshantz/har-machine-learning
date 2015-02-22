
---
title: "Prediction Assignment: Human Activity Recognition in Weight Lifting Exercises"
author: "Jeremy Shantz"
date: '2015-02-22'
output: html_document
---

Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5.

In this study we use machine learning to predict the execution quality of test subjects performing [unilateral dumbbell biceps curl](https://www.youtube.com/results?search_query=Unilateral+Dumbbell+Biceps+Curl) using data [[1]](#Velloso) from sensors worn by the subjects. From the data's 160 variables we select 52 features and apply a random forest algorithm to construct a statistical model. We apply that model to test and validation subsets to predict whether each observation corresponds to the correct execution of the exercise or to one of five ways it can be executed incorrectly.

More information about the dat is available: [Weight Lifting Exercises Dataset](http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises).
http://groupware.les.inf.puc-rio.br/public/papers/2013.Velloso.QAR-WLE.pdf

## Building the model

### Training and Test Sets
Load the raw training and testing data sets.

```r
rawData <- read.csv('./data/pml-training.csv')
finalTestData <- read.csv('./data/pml-testing.csv')
```

Partition the training data into training, testing, validation data sets.
Sixty percent of the data is used to train our model, reserving 40% of the data to test against so we can measure the effectiveness of our model. The test data is further divided equally between testing and validation sets.


```r
indexes <- createDataPartition(y=rawData$classe, p=0.60, list=FALSE)
train.original <- rawData[indexes,]
testset <- rawData[-indexes,]
test.indexes <- createDataPartition(y=testset$classe, p=0.50, list=FALSE)
test <- testset[test.indexes,] 
validate <- testset[-test.indexes,] 
```

Random sample from the data:

```r
head(train[, sample(names(train.original), 38)],1)
```

```
## Error in `[.data.frame`(train, , sample(names(train.original), 38)): undefined columns selected
```

### Feature Selection
With 160 potential features, our first task is to remove low-value data. We
start by removing the first 7 columns which are clearly not predictors.


```r
train <- train.original[, -7:-1]
```

Next we remove columns with little variation since they will not be useful in
explaining the variation we observe in the `classe` variable.


```r
nzvar <- nearZeroVar(train, saveMetrics=TRUE)
train <- train[, !(nzvar$nzv)]
```

There remain many columns which primarily contain the NA value. Quantify the NAs by calculating the count of NAs in each column.


```r
sapply(1:ncol(train) - 1, function(i) { sum(is.na(train[,i]))  })
```

```
##   [1]     0     0     0     0     0 11524 11524 11524 11524 11524 11524 11524 11524 11524 11524
##  [16] 11524 11524 11524 11524 11524 11524     0     0     0     0     0     0     0     0     0
##  [31]     0     0     0     0 11524     0     0     0     0     0     0     0     0     0 11524
##  [46] 11524 11524 11524 11524 11524 11524 11524 11524     0     0     0 11524 11524 11524 11524
##  [61] 11524 11524     0 11524 11524 11524 11524 11524 11524 11524 11524 11524 11524     0     0
##  [76]     0     0     0     0     0     0     0     0     0     0 11524 11524 11524 11524 11524
##  [91]     0 11524     0     0     0     0     0     0     0     0     0
```

Remove all columns where the count of NA is non-zero.


```r
indexes <- sapply(1:ncol(train), function(i) { ifelse(sum(is.na(train[,i])) > 0, -i, i)  })
train <- train[, indexes[indexes < 0]]
```

### Random Forest

With a set of 52 predictors in hand, we train a model using a random forest. Our outcome variable is `classe` (containing five possible values A, B, C, D, E) and the model is trained against the ` nrow(train) ` observations in the training data set.

```r
#model <- train(classe ~., data=train, method="rf", tuneGrid=data.frame(mtry=2) )
```

When applied to the training data set, the model's accuracy is 99.8%.


```r
predictions <- predict(model, train)
mt <- confusionMatrix(predictions, train$classe)
mt$overall[1:5]
```

```
##      Accuracy         Kappa AccuracyLower AccuracyUpper  AccuracyNull 
##     0.9965183     0.9955961     0.9952797     0.9975004     0.2843071
```

## Cross validation

The random forest algorithm internally uses a bootstrapped resampling strategy for cross validation. In this case, it made 25 repetitions using the 'boot' method.


```r
modelFit$control$method
```

```
## Error in eval(expr, envir, enclos): object 'modelFit' not found
```

```r
modelFit$control$repeats
```

```
## Error in eval(expr, envir, enclos): object 'modelFit' not found
```

### Out of Sample Error Rate - From RF's Bootstrap resampling

The model predicts an out of sample error rate of 0.91%.


```r
modelFit$finalModel
```

```
## Error in eval(expr, envir, enclos): object 'modelFit' not found
```

```r
modelFit
```

```
## Error in eval(expr, envir, enclos): object 'modelFit' not found
```
 
### Out of Sample Error Rate - Applying the Model to the Test Data

We can now apply the prediction algorithm to the test set. Because it was not used to train the model, it is an unbiased measure of the out of sample error rate.


```r
testResults <- confusionMatrix(predict(model, test), test$classe)
testResults$overall[1:6]
```

```
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 
##      0.9961764      0.9951634      0.9937014      0.9978584      0.2844762      0.0000000
```

```r
testResults$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 1115    3    0    0    0
##          B    0  755    0    0    0
##          C    1    1  684    6    2
##          D    0    0    0  637    2
##          E    0    0    0    0  717
```

```r
errorRate <- round(1 - testResults$overall[[1]], 4)
paste('The error rate is', errorRate, '%')
```

```
## [1] "The error rate is 0.0038 %"
```







 
# Test Cases 
2. You should also apply your machine learning algorithm to the 20 test cases available in the test data above. Please submit your predictions in appropriate format to the programming assignment for automated grading. See the programming assignment for additional details. 
You will also use your prediction model to predict 20 different test cases. 

# Future Studies
only the dumbbell sensor, the Kinect data

# Citation

<div id="Velloso"></div>
Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. [Qualitative Activity Recognition of Weight Lifting Exercises](http://groupware.les.inf.puc-rio.br/work.jsf?p1=11201). Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013. 


<!-- Add extra space below to make the citation link more obvious -->
<div style="height:300px;"></div>
