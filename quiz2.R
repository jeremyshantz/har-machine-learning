setwd('~/courses/practical_machine_learning/')

# Question 1

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

rm(list=ls())

# Question 2

library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)
library(ggplot2)
library(gridExtra)
data(concrete)

set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

plot(mixtures$CompressiveStrength, inTrain)

summary(training)
featurePlot(x = training[, c('Age', 'FlyAsh', 'Water')],
            y= training$CompressiveStrength, 
            plot="pairs")
plot(CompressiveStrength ~ Age, data= training)
pairs(training)
qplot(Age, CompressiveStrength, data= training)

qq <- qplot(Cement, CompressiveStrength, data= training, colour=FineAggregate)
qq + geom_smooth(method='lm', formula=y~x)

cuts <- cut2(training$CompressiveStrength, g=12)
table(cuts)

qplot(cuts, CompressiveStrength, data= training, colour=Cement)

p1 <- qplot(cuts, CompressiveStrength, data=training, fill=cuts, colour=Age, geom=c('boxplot'))
p2 <- qplot(cuts, Cement, data=training, fill=cuts, geom=c('boxplot', 'jitter'))
grid.arrange(p1, p2, ncol=2)

# doesn't work well will continuous variables
table(cuts, training$Cement)

qplot(Cement, color=cuts, data= training, geom=c('density'))


# near Zero variable
hist(training$Superplasticizer)
nearZeroVar(training, saveMetrics = TRUE)

# principal components analysis

training$strength <- training$CompressiveStrength > mean(training$CompressiveStrength)
typecolor <- ((training$strength == TRUE)*1+1)
com <- prcomp(log10(training[, c(1:8)] + 1))
com$rotation
plot(com$x[,1], com$x[,2], xlab='PC1', ylab='PC2', col=typecolor)



library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

il.variable.names <- grep('^IL', names(training))
preProc <- preProcess(training[, il.variable.names], method='pca', pcaComp=11)
trainPC <- predict(preProc, training[,il.variable.names])
modelFit <- train(training$diagnosis ~ ., method='glm', data=trainPC)

confusionMatrix(training$diagnosis, predict(modelFit,trainPC))

testPC <- predict(preProc, testing[, il.variable.names])
confusionMatrix(testing$diagnosis, predict(modelFit,testPC))

modelFit
