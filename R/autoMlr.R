autoMlr = function(task, runtime) { 
  # add algorithms as input
  
  library(mlr)
  type = getTaskType(task)
  lrn.name = paste0(type, ".glmnet")
  lrn = makeLearner(cl = lrn.name, par.vals = as.list(defaults$glmnet$default[1,]))
  mod = train(lrn, task)

  # autoxgboost
  # tuneRanger
  
  return(mod)
}