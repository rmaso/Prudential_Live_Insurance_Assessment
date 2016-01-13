library(caret)
library(xgboost)
library(randomForest)
library(party)

load('varImp.RData')

basicTC <- trainControl(method = "cv", 
                         number = 2,
                         verboseIter = TRUE)


executeModel <- function(x, y, method, ...){
  model <- train(x, 
                 y, 
                 method = method, 
                 preProcess = c("zv"),
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

# models.general <- function(x, y, method){
models.general <- function(training, testing, method){
  x <- subset(training, select = rownames(importance))
  x <- levelsMakeNames(x)
  y <- training$Response
  levels(y) <- make.names(levels(y))

  model <- executeModel(x, y,
                        method = method,
                        trControl = basicTC)
  
  confMatrix <- predict.general(model = model$finalModel, 
                       method=method,
                       x=subset(testing, select = rownames(importance)), 
                       y=testing$Response)
  
  list(model=model, confMatrix = confMatrix)
}

predict.general <- function(model, x, y, method, ...){
  x <- levelsMakeNames(x)
  levels(y) <- make.names(levels(y))
  
  pred <- predict(model, newdata = x, ...)
  if(is.data.frame(pred)){
    pred <- pred$class
  }
  cm <- confusionMatrix(as.character(pred), as.character(y))
  
  writeModel(cm, method, 'confMatrix')
  cm
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