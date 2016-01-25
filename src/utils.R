
if(DEBUG){
  paths = list(data='../data/',
               submit='../debug_submissions/',
               models='../debug_models/',
               r='../R/')
}else{
  paths = list(data='../data/',
               submit='../submissions/',
               models='../models/',
               r='../R/')
}


discretizeVars <- function(x){
  library(arules)
  
  if(is.factor(x)){
    temp <- NULL
  }else if(length(unique(x)) < 10){
    temp <- 'factor'
  }else{
    temp <- discretize(x, "frequency", categories=4, onlycuts=TRUE)
  }
  temp
}

applyDiscretize <- function(z, cut){
  library(arules)

  if(is.null(cut)){
    temp <- z
  }else if(is.character(cut)){
    temp <- data.frame(as.factor(z[[1]]))
  }else{
    z[which(z < min(cut))] <- min(cut)
    z[which(z > max(cut))] <- max(cut)
    temp <- discretize(z[[1]], "fixed", categories=cut, labels=paste0("X", 1:length(cut)))
    temp <- as.data.frame(temp)
  }
  colnames(temp) <- colnames(z)
  temp
}


levelsMakeNames <- function(df){
  as.data.frame(lapply(df, function(x)
    if("factor" %in% class(x) ) { 
      levels(x) <- make.names(levels(x))
      x } else x  ))  
}

levelsMoreThan <- function(df, n){
  sapply(df, function(x) length(levels(x)) > n)
}

writeModel <- function(model, id, pref){
  subs <- dir(paths$models)
  subs <- grep(paste(pref,'_',id,'_','[0-9]+(.RData)?', sep=""), subs, value=TRUE)
  nums <- gsub(paste(pref,'_',id,'_',sep=""),'', gsub('(.RData)?','', subs))
  if(length(nums) == 0){
    model.number <- 1
  }else{
    model.number <- max(as.numeric(nums)) + 1
  }
  submit.path = paste0(paths$models, 
                       pref,
                       '_',
                       id,
                       '_',
                       model.number,
                       '.RData')
  print(paste('Writing to:', submit.path))
  save(model, file = submit.path)
  model.number
}
