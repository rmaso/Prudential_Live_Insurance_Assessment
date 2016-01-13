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
