#' autoMlr
#'
#' @param task 
#' @param runtime 
#'
#' @return
#' @export
#'
#' @examples
autoMlr = function(task, runtime) { 
  # add algorithms as input
  
  p = getTaskNFeats(task)
  defaults$ranger$default$mtry = ceiling(p * defaults$ranger$default$mtry)
  n = getTaskSize(task)
  defaults$ranger$default$min.node.size = round(n^defaults$ranger$default$min.node.size)
    
  library(mlr)
  type = getTaskType(task)
  #lrn.name = ifelse(type == "regr", "regr.lm", "classif.logreg")
  #lrn0 = makeLearner(cl = lrn.name, predict.type = "prob")
  lrn.name = paste0(type, ".glmnet")
  lrn1 = makeLearner(cl = lrn.name, par.vals = as.list(defaults$glmnet$default[1,]), predict.type = "prob")
  lrn.name = paste0(type, ".rpart")
  lrn2 = makeLearner(cl = lrn.name, par.vals = as.list(defaults$rpart$default[1,]), predict.type = "prob")
  lrn.name = paste0(type, ".kknn")
  lrn3 = makeLearner(cl = lrn.name, par.vals = as.list(defaults$kknn$default[1,, drop = F]), predict.type = "prob")
  lrn.name = paste0(type, ".svm")
  lrn4 = makeLearner(cl = lrn.name, par.vals = as.list(defaults$svm$default[1,][,!is.na(defaults$svm$default[1,])]), predict.type = "prob")
  lrn.name = paste0(type, ".ranger")
  lrn5 = makeLearner(cl = lrn.name, par.vals = as.list(defaults$ranger$default[1,]), predict.type = "prob")
  lrn.name = paste0(type, ".xgboost")
  lrn6 = makeLearner(cl = lrn.name, par.vals = as.list(defaults$xgboost$default[1,]), predict.type = "prob")

  # makeStackedLearner not modular enough. Change this!
  lrn = makeStackedLearner(base.learners = list(lrn1, lrn2, lrn3, lrn4, lrn5, lrn6), predict.type = "prob", method = "hill.climb")
  mod = train(lrn, task)
  
  # Zeitbeschränkung:
  # Fange mit super einfachen, kleinen Modellen an, funktionieren sie, wie lange brauchen sie?
  
  
  # resampling = makeResampleDesc("RepCV", reps = 5, folds = 5)
  # res = benchmark(list(lrn,lrn5), task, resampling, measures = list(mmce, multiclass.au1p, multiclass.brier, logloss))
  # res

  # autoxgboost
  # tuneRanger
  
  return(mod)
}


