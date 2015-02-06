setwd('~/courses/practical_machine_learning/project/')
library('caret')
library('ggplot2')
set.seed(32343)

train <- read.csv('./data/pml-training.csv')

inTrain <- createDataPartition(y=train$classe, p=0.75, list=FALSE)
training <- train[inTrain,]
testing <- train[-inTrain,]
dim(training)

modelFit <- train(classe ~.,data=training, method="glm")
smodelFit
modelFit$finalModel

predictions <- predict(modelFit,newdata=testing)
predictions

confusionMatrix(predictions,testing$type)

# splitting k-fold for cross-validation
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,returnTrain=FALSE)
sapply(folds,length)
folds[[1]][1:10]

# resampling with replacement
set.seed(32323)
folds <- createResample(y=spam$type,times=10,
                        list=TRUE)
sapply(folds,length)
folds[[1]][1:10]


# exploratory plotting
featurePlot(x=train[, c('gyros_forearm_x', 'magnet_forearm_y', 'var_pitch_forearm')],
            y = train$classe,
            plot='pairs'
)

qplot(var_pitch_forearm, classe, data=train)

# http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises
# belt, forearm, arm, dumbell  
grep('belt', names(training), value=TRUE)


jeremy <- training[training$user_name=="jeremy", ]




