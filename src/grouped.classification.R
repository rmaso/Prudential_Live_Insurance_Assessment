library(caret)
library(xgboost)
library(randomForest)
library(party)

basicTC <- trainControl(method = "cv", 
                         number = 2,
                         verboseIter = TRUE)


executeModel <- function(x, y, method, ...){
  model <- train(x, y, 
                   method = method, 
                   ...)
  writeModel(model, method)
  model
}

rmcXGB <- function(x, y, ...){
  model <- xgboost(data      = data.matrix(x),
                 label       = y,
                 ...)
  model
}


models.rf <- function(x, y){
  model <- executeModel(x, 
                        y, 
                        trControl = basicTC)
  model
}

models.rFerns <- function(x, y){
  x <- levelsMakeNames(x)
  levels(y) <- make.names(levels(y))
  
  model <- executeModel(x, 
                         y, 
                         method = "rFerns",
                         ferns = 50,
                         trControl = basicTC)
  model
}

models.general <- function(x, y, method){
  x <- levelsMakeNames(x)
  levels(y) <- make.names(levels(y))

  model <- executeModel(x, y,
                        method = method,
                        trControl = basicTC)
  model
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