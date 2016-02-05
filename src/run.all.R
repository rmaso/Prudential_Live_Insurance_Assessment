library(data.table)
library(caret)

## TODO: Remove to publish
setwd('Documents/Kaggle/Prudential_Live_Insurance_Assessment/src/')
DEBUG = F

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

if(file.exists('inTrain.RData')){
  load('inTrain.RData')
}else{
  inTrain <- createDataPartition(train$Response, p=0.6, list=FALSE)
  save(inTrain, 'inTrain.RData')  
}
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
sum(is.na(test))
test[is.na(test)] <- 0

######### Models Variables ###########

if(file.exists('cuts.RData')){
  load('cuts.RData')
}else{
  cuts <- lapply(subset(training, select = rownames(importance)), discretizeVars)
  save(cuts, file='cuts.RData')  
}


models <- list()

modelList <- list( list(model="AdaBag", tuneGrid = expand.grid(mfinal = (1:3)*50, 
                                                               maxdepth = c(1, 3, 6, 9))),
                   list(model='treebag', tuneGrid = NULL),
                   list(model='nb', tuneGrid = NULL),
                   list(model='treebag', tuneGrid = expand.grid(fL = (0:5)*5, 
                                                                usekernel = c(TRUE, FALSE))),
                   list(model='rpart', tuneGrid = NULL),
                   list(model='rpart2', tuneGrid = NULL),
                   list(model='rf', tuneGrid = NULL),
                   list(model='rfRules', tuneGrid = NULL),
                   list(model='RRF', tuneGrid = NULL),
                   list(model='LMT', tuneGrid = NULL),
                   list(model='LogitBoost', tuneGrid = NULL),
                   list(model='svmLinear2', tuneGrid = NULL),
                   list(model='lda', tuneGrid = NULL),
                   list(model='lda2', tuneGrid = NULL),
                   list(model='lssvmRadial', tuneGrid = NULL),
                   list(model='pda', tuneGrid = NULL),
                   list(model='pda2', tuneGrid = NULL),
                   list(model='qda', tuneGrid = NULL),
                   list(model='rFerns', tuneGrid = NULL),
                   list(model='Boruta', tuneGrid = NULL),
                   list(model='nnet', tuneGrid = NULL),
                   list(model='pcaNNet', tuneGrid = NULL),
                   list(model='avNNet', tuneGrid = NULL)
)


modelList.t <- modelList[1:2]
models <- lapply(modelList, 
                 function(m, training, testing){
                   try(models.general(training, testing, method=m$model, tuneGrid = m$tuneGrid))
                 }, training, testing)

# Bagged AdaBoost	AdaBag	Classification	adabag, plyr	mfinal, maxdepth
models[[length(models)+1]] <- models.general(training, testing, 
                                             method = 'AdaBag', 
                                             tuneGrid = expand.grid(mfinal = (1:3)*50, 
                                                                    maxdepth = c(1, 3, 6, 9)))

# Bagged CART	treebag	Dual Use	ipred, plyr, e1071	None
models[[length(models)+1]] <- models.general(training, testing, 
                                             method = 'treebag')

# Naive Bayes	nb	Classification	klaR	fL, usekernel
models[[length(models)+1]] <- models.general(training, testing, 
                                             method = 'nb', 
                                             tuneGrid = expand.grid(fL = (0:5)*5, 
                                                                    usekernel = c(TRUE, FALSE)))

# CART	rpart	Dual Use	rpart	cp
models[[length(models)+1]] <- models.general(training, testing, # TODO: Test discretize = TRUE
                                             method = 'rpart')  # TODO: tuneGrid with cp between 0 and 1 by 0.2

# CART	rpart2	Dual Use	rpart	maxdepth
models[[length(models)+1]] <- models.general(training, testing, # TODO: Test discretize = TRUE
                                             method = 'rpart2') 

# Random Forest	rf	Dual Use	randomForest	mtry
models[[length(models)+1]] <- models.general(training, testing, # TODO: Test discretize = TRUE
                                             method = 'rf')

# Random Forest Rule-Based Model	rfRules	Dual Use	randomForest, inTrees, plyr	mtry, maxdepth
models[[length(models)+1]] <- models.general(training, testing, # TODO: Test discretize = TRUE
                                             method = 'rfRules')

# Regularized Random Forest	RRF	Dual Use	randomForest, RRF	mtry, coefReg, coefImp
models[[length(models)+1]] <- models.general(training, testing, # TODO: Test discretize = TRUE
                                             method = 'RRF')    # Lento

# Support Vector Machines with Linear Kernel	svmLinear	Dual Use	kernlab	C
models[[length(models)+1]] <- models.general(training, testing, 
                                             method = 'svmLinear')

# Support Vector Machines with Linear Kernel	svmLinear2	Dual Use	e1071	cost, gamma
models[[length(models)+1]] <- models.general(training, testing, 
                                             discretize = TRUE,
                                             method = 'svmLinear2')

# Logistic Model Trees	LMT	Classification	RWeka	iter
models[[length(models)+1]] <- models.general(training, testing, 
                                             method = 'LMT')

# Boosted Logistic Regression  LogitBoost  Classification	caTools	nIter
models[[length(models)+1]] <- models.general(training, testing, 
                                             method = 'LogitBoost')

# Linear Discriminant Analysis	lda	Classification	MASS	None
models[[length(models)+1]] <- models.general(training, testing,
                                             discretize = TRUE,
                                             method = 'lda')

# Linear Discriminant Analysis	lda2	Classification	MASS	dimen
models[[length(models)+1]] <- models.general(training, testing,
                                             discretize = TRUE,
                                             method = 'lda2')

# Least Squares Support Vector Machine with Radial Basis Function Kernel	lssvmRadial	Classification	kernlab	sigma
models[[length(models)+1]] <- models.general(training, testing,
                                             discretize = TRUE,
                                             method = 'lssvmRadial')

# Penalized Discriminant Analysis	pda	Classification	mda	lambda
models[[length(models)+1]] <- models.general(training, testing,
                                             discretize = TRUE,
                                             method = 'pda')

# Penalized Discriminant Analysis	pda2	Classification	mda	df
models[[length(models)+1]] <- models.general(training, testing,
                                             discretize = TRUE,
                                             method = 'pda2')

# Quadratic Discriminant Analysis	qda	Classification	MASS	None
models[[length(models)+1]] <- models.general(training, testing,
                                             method = 'qda')

# Random Ferns	rFerns	Classification	rFerns	depth
models[[length(models)+1]] <- models.general(training, testing,
                                             method = 'rFerns')

# Random Forest with Additional Feature Selection	Boruta	Dual Use	Boruta, randomForest	mtry
models[[length(models)+1]] <- models.general(training, testing,
                                             method = 'Boruta')

# 
models[[length(models)+1]] <- models.general(training, testing, 
                                             method = 'nnet')

models[[length(models)+1]] <- models.general(training, testing, 
                                             method = 'pcaNNet')

models[[length(models)+1]] <- models.general(training, testing, 
                                             method = 'avNNet')





models[[length(models)+1]] <- models.general(training, testing, 
                                             method = 'bagFDA')
#predictions failed for Fold1: nprune= 2, degree=1 Error in eval(expr, envir, enclos) : 
#  object 'Medical_History_4X1' not found

models[[length(models)+1]] <- models.general(training, testing,
                                             discretize = TRUE,
                                             method = 'nbDiscrete')
#model fit failed for Fold1: smooth=0 Error : is_dag_graph(dag) is not TRUE




predTrainAll <- predict.all.models(training)
predTrainAll <- as.data.frame(lapply(predTrainAll, factor, levels=c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")))


predTestingAll <- predict.all.models(testing)
predTestingAll <- as.data.frame(lapply(predTestingAll, factor, levels=c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")))

model <- train(predTrainAll, training$Response, method = "rf",
               trControl = trainControl(method = "cv", 
                                        number = 10,
                                        verboseIter = TRUE))

pred <- predict(model$finalModel, newdata=predTestingAll, type="class")
confusionMatrix(as.character(pred), as.character(testing$Response))


x <- as.matrix(data.frame(lapply(predTrainAll,as.numeric)))
model <- train(x, training$Response, method = "svmLinear",
               trControl = trainControl(method = "cv", 
                                        number = 10,
                                        verboseIter = TRUE))

x <- as.matrix(data.frame(lapply(predTestingAll,as.numeric)))
pred <- predict(model$finalModel, newdata=x, type="response")
confusionMatrix(as.character(pred), as.character(testing$Response))

modelNoResume <- train(predTrainAll, training$Response, 
                                     method = "nnet",
                                     trControl = trainControl(method = "cv", 
                                                              number = 3,
                                                              verboseIter = TRUE),
                                    
                                     maxit = 1000)

pred <- predict(modelNoResume$finalModel, newdata=predTestingAll, type="class")
confusionMatrix(as.character(pred), as.character(testing$Response))


# predAllTrain <- predict.all.models(training)
library(Matrix)
resumeMatrix <- t(apply( as.matrix(predTrainAll), 1, function (x){
  table(factor(x, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8"))) 
}))

modelResume <- train(resumeMatrix, training$Response, 
                      method = "nnet", 
                      preProcess = NULL,
                      maxit = 1000,
                      trControl = trainControl(method = 'cv',
                                               number = 3,
                                               verboseIter = TRUE)
                      )


resumeMatrixTesting <- t(apply( as.matrix(predTestingAll), 1, function (x){
  table(factor(x, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8"))) 
}))

pred <- predict(model$finalModel, newdata=resumeMatrixTesting, type="class")
confusionMatrix(as.character(pred), as.character(testing$Response))

save(modelNoResume, modelResume, file="finalModels.RData")


# Corregir el unico caso donde Medical_History_33 == 2
test$Medical_History_33[test$Medical_History_33 == 2] <- 3
test$Medical_History_33 <- factor(test$Medical_History_33)

testModif <- as.data.frame(sapply(names(test)[2:length(names(test))], function(x){
  tmp <- as.data.frame(training[,x,with=FALSE])
  colnames(tmp) <- c("col")
  tmp2 <- as.data.frame(test[,x, with=FALSE])
  if(is.factor(tmp$col)){
    colnames(tmp2) <- c("col")
    tmp2 <- factor(tmp2$col, levels=levels(tmp$col))
  }
  tmp2
}))


predTestAll <- predict.all.models(testModif)
predTestAll <- as.data.frame(lapply(predTestAll, factor, levels=c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")))



predNoResume <- predict(modelNoResume$finalModel, newdata=predTestAll, type="class")

resumeMatrixTest <- t(apply( as.matrix(predTestAll), 1, function (x){
  table(factor(x, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8"))) 
}))


predResume <- predict(modelResume$finalModel, newdata=resumeMatrixTest, type="class")

write.submission( data.frame(Id = test$Id, Response = predNoResume), "NoResume")
write.submission( data.frame(Id = test$Id, Response = predResume), "Resume")







































# Linear Discriminant Analysis  lda	Classification	MASS	None
# Linear Discriminant Analysis	lda2	Classification	MASS	dimen
# Least Squares Support Vector Machine	lssvmLinear	Classification	kernlab	None
# Least Squares Support Vector Machine with Polynomial Kernel	lssvmPoly	Classification	kernlab	degree, scale
# Semi-Naive Structure Learner Wrapper	nbSearch	Classification	bnclassify	k, epsilon, smooth, final_smooth, direction
# Penalized Discriminant Analysis	pda	Classification	mda	lambda
# Penalized Discriminant Analysis	pda2	Classification	mda	df
# Quadratic Discriminant Analysis	qda	Classification	MASS	None
# Random Ferns	rFerns	Classification	rFerns	depth
# Random Forest with Additional Feature Selection	Boruta	Dual Use	Boruta, randomForest	mtry

# Robust Quadratic Discriminant Analysis  QdaCov  Classification	rrcov	None
# Naive Bayes Classifier with Attribute Weighting  awnb  Classification	bnclassify	smooth











basicTC <- trainControl(method = "cv", 
                        number = 2,
                        verboseIter = TRUE)


models[[length(models)+1]] <- models.general(training, testing, method = 'bagEarth') # Not Working


models[[length(models)+1]] <- models.rf(subset(training[ , !levelsMoreThan(training, 53), with=FALSE], select=-Response), training$Response)
models[[length(models)+1]] <- models.rFerns(subset(training[ , !levelsMoreThan(training, 30), with=FALSE], select=-Response), training$Response)

mb <- models.bag(subset(training, select=-Response), training$Response)

# models[[length(models)+1]] <- models.general(subset(training[ , !levelsMoreThan(training, 30), with=FALSE], select=-Response), training$Response, method = 'awtan')

xgb <- models.xgb(subset(training, select=-Response), training$Response)
