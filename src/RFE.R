library(caret)
library(data.table)
colClasses=c("integer","factor","factor","factor","numeric","factor","factor","factor",
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
train <- read.csv("../data/train.csv", colClasses=colClasses, stringsAsFactors=TRUE)
test <- fread("../data/test.csv", colClasses = colClasses[-length(colClasses)], stringsAsFactors=TRUE)

train <- as.data.frame(train)
test <- as.data.frame(test)

ttt <- sapply(names(test), function(x){
  if(is.factor(train[,x])){
      setdiff(unique(as.character(train[,x])), unique(as.character(test[,x])))
  }else NULL
})
names(ttt) <- names(test)


source('./utils.R')
library(caret)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   number = 10,
                   repeats = 5,
                   verbose = TRUE)

x <- subset(train, select=-c(Id, Response))
x <- x[, !levelsMoreThan(x, 53)]
y <- as.factor(train$Response)
x[is.na(x)] <- 0

subsets <- c(seq(5, 20, by=3),seq(25,100, by=5))
library(randomForest)
lmProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile
plot(lmProfile)