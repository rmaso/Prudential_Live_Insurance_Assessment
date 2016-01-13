library(data.table)
library(caret)

## TODO: Remove to publish
setwd('Documents/Kaggle/Prudential_Live_Insurance_Assessment/src/')
DEBUG = TRUE

colClasses=c("integer","factor","factor","factur","numeric","factor","factor","factor",
             "numeric","numeric","numeric","numeric","numeric","factor","factor","numeric","factor","numeric",
             "factor","factor","factor","factor","factor","factor","factor",
             "factor","factor","factor","factor","numeric","factor","factor","factor",
             "factor","numeric","numeric","numeric","numeric",
             "integer","factor","factor","factor","factor","factor","factor","factor","factor","factor",
             "factor","factor","factor","factor",
             "integer","factor","factor","factor","factor","factor","factor","factor","factor",
             "integer","factor","factor","factor","factor","factor","factor","factor",
             "integer","factor","factor","factor","factor","factor","factor","factor","factor","factor",
             "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer",
             "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer",
             "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer",
             "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer",
             "integer","integer","integer","integer","integer","integer","integer","integer","factor")
train <- fread("../data/train.csv", colClasses=colClasses, stringsAsFactors=TRUE)
test <- fread("../data/test.csv", colClasses = colClasses[-length(colClasses)], stringsAsFactors=TRUE)

inTrain <- createDataPartition(train$Response, p=0.6, list=FALSE)
training <- subset(train[inTrain, ], select=-Id)
testing <- subset(train[-inTrain, ], select=-Id)

if(DEBUG){
  training <- training[1:1000, ]
  testing <- testing[1:1000, ]
}

source('./grouped.classification.R')
source('./utils.R')
sum(is.na(training))
training[is.na(training)] <- 0
sum(is.na(testing))
testing[is.na(testing)] <- 0


models <- list()

models[[length(models)+1]] <- models.general(training, testing, method = 'AdaBag')
models[[length(models)+1]] <- models.general(training, testing, method = 'bagEarth') # Not Working
models[[length(models)+1]] <- models.general(training, testing, method = 'treebag')
models[[length(models)+1]] <- models.general(training, testing, method = 'bagFDA')
models[[length(models)+1]] <- models.general(training, testing, method = 'lda')
models[[length(models)+1]] <- models.general(training, testing, method = 'nb')
models[[length(models)+1]] <- models.general(training, testing, method = 'rpart')


models[[length(models)+1]] <- models.rf(subset(training[ , !levelsMoreThan(training, 53), with=FALSE], select=-Response), training$Response)
models[[length(models)+1]] <- models.rFerns(subset(training[ , !levelsMoreThan(training, 30), with=FALSE], select=-Response), training$Response)

mb <- models.bag(subset(training, select=-Response), training$Response)

# models[[length(models)+1]] <- models.general(subset(training[ , !levelsMoreThan(training, 30), with=FALSE], select=-Response), training$Response, method = 'awtan')

xgb <- models.xgb(subset(training, select=-Response), training$Response)
