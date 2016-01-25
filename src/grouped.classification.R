library(caret)
library(xgboost)
library(randomForest)
library(party)

library(adabag)
library(klaR)
library(randomForest)
library(rpart)
library(kernlab)
library(ipred)
library(plyr)
library(e1071)
library(RWeka)

load('varImp.RData')
## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite("graph")


if(DEBUG){
  basicTC <- trainControl(method = "cv", 
                          number = 2,
                          classProbs = TRUE, 
                          verboseIter = TRUE)
}else{
  basicTC <- trainControl(method = "cv", 
                          number = 10,
                          verboseIter = TRUE)
}

executeModel <- function(x, y, method, ...){
  model <- train(x, 
                 y, 
                 method = method, 
                 preProcess = c("zv"),
                 tuneLength = 10, 
                 ...)

  writeModel(model, method, 'model')
  model
}

rmcXGB <- function(x, y, ...){
  model <- xgboost(data      = data.matrix(x),
                 label       = y,
                 ...)
  model
}

# models.general <- function(x, y, method){
models.general <- function(training, testing, method, discretize = FALSE, ...){
  x <- subset(training, select = rownames(importance))
  if(discretize){
    cuts <- lapply(x, discretizeVars)
    
    x <- as.data.frame(lapply(names(cuts), function(name, x, cuts){
                                              applyDiscretize(x[,name, with=FALSE], cuts[[name]])
          }, x, cuts))
  }
  
  x <- levelsMakeNames(x)
  y <- training$Response
  levels(y) <- make.names(levels(y))
  
  if(grepl("svm", method) | grepl("lda", method) | grepl("pda", method) | grepl("qda", method) | grepl("Logit", method)){
    library(Matrix)
    x <- as.matrix(data.frame(lapply(x,as.numeric)))
  }

  model <- suppressWarnings(executeModel(x, y,
                                          method = method,
                                          trControl = basicTC, ...))

  x <- subset(testing, select = rownames(importance))
  if(discretize){
    x <- as.data.frame(lapply(names(cuts), function(name, x, cuts){
                                              applyDiscretize(x[,name, with=FALSE], cuts[[name]])
                                            }, x, cuts))
  }
  x <- levelsMakeNames(x)
  if(grepl("svm", method) | grepl("lda", method) | grepl("pda", method) | grepl("qda", method) | grepl("Logit", method)){
    library(Matrix)
    x <- as.matrix(data.frame(lapply(x,as.numeric)))
  }
  

  y <- testing$Response
  levels(y) <- make.names(levels(y))
  
    
  confMatrix <- predict.general(model = model$finalModel, 
                       method=method,
                       x=x, 
                       y=y)
  
  list(model=model, confMatrix = confMatrix)
}

predict.general <- function(model, x, y, method, ...){
  pred <- predict(model, newdata = x, type=ifelse(grepl("svm", method), "response", "class"), ...)
  if(is.list(pred)){
    pred <- pred$class
  }
  cm <- suppressWarnings(confusionMatrix(as.character(pred), as.character(y)))
  
  writeModel(cm, method, 'confMatrix')
  cm
}

predict.all.models <- function(testing){
  # fileNameList <- paste(paths$models, dir(paths$models, pattern = "^model"), sep="")
  fileNameList <- paste("../models/", dir("../models/", pattern = "^model"), sep="")
  dataList <- lapply(fileNameList, predict.testing, testing=testing)
  totalData <- do.call("cbind", dataList)
  totalData
}

predict.testing <- function(modelFile, testing, discretize = FALSE){
  cat(modelFile, "\n")
  x <- subset(testing, select = rownames(importance))
  if(discretize){
    x <- as.data.frame(lapply(names(cuts), function(name, x, cuts){
      applyDiscretize(x[,name, with=FALSE], cuts[[name]])
    }, x, cuts))
  }
  x <- levelsMakeNames(x)
  load(modelFile)
  method <- model$method
  if(grepl("svm", method) | grepl("lda", method) | grepl("pda", method) | grepl("qda", method) | grepl("Logit", method)){
    library(Matrix)
    x <- as.matrix(data.frame(lapply(x,as.numeric)))
  }
  
  pred <- predict(model$finalModel, newdata=x, type=ifelse(grepl("svm", method), "response", "class"))
  if(is.list(pred)){
    pred <- pred$class
  }
  as.character(pred)
  
}

models.bag <- function(x, y){
  x <- levelsMakeNames(x)
  levels(y) <- make.names(levels(y))
  model <- executeModel(x, 
                        y, 
#                       B = 10, 
#                       trControl = trainControl(method = "cv", 
#                                                number = 3, 
#                                                classProbs = TRUE),
                      bagControl = bagControl(fit = ldaBag$fit,
                                              predict = ldaBag$pred,
                                              aggregate = ldaBag$aggregate))
#                       tuneGrid = data.frame(vars = seq(1, 15, by = 2)))
  model
}

# treebag <- bag(predictors, temperature, B = 10,
#                bagControl = bagControl(fit = ctreeBag$fit,
#                                        predict = ctreeBag$pred,
#                                        aggregate = ctreeBag$aggregate))


models.XGB <- function(x, y){
  model <- rmcXGB(x,
                  y,
                  nrounds     = 100,
                  objective   = "reg:linear",
                  eval_metric = "rmse")
}