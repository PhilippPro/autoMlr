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
  
  type = getTaskType(task)
  lrn = list()
  #lrn.name = ifelse(type == "regr", "regr.lm", "classif.logreg")
  #lrn0 = makeLearner(cl = lrn.name, predict.type = "prob")
  # use multiple defaults!
  lrn.names = paste0(type, ".", c("glmnet", "rpart", "kknn", "liquidSVM")) #, "tuneRanger")) , "autoxgboost"))
  lrn[[1]] = makeLearner(cl = lrn.names[[1]], par.vals = as.list(defaults$glmnet$default[1,]))
  lrn[[2]] = makeLearner(cl = lrn.names[[2]], par.vals = as.list(defaults$rpart$default[1,]))
  lrn[[3]] = makeLearner(cl = lrn.names[[3]], par.vals = as.list(defaults$kknn$default[1,, drop = F]))
  lrn[[4]] = makeLearner(cl = lrn.names[[4]])
  #lrn[[5]] = makeLearner(cl = lrn.names[[5]], par.vals = list(time.budget = 900))
  #lrn[[6]] = makeLearner(cl = lrn.names[[6]], par.vals = list(time.budget = 1800))

  # make weighted learner?
  mod = list()
  for(i in 1:length(lrn))
    mod[[i]] = train(lrn[[i]], task)
  
  # super learner
  # better make the training on a holdout-dataset (blending) or with CV?! (should be a parameter)
  # not suitable for small datasets
  # class probabilities are much better (liquidSVM?) -> Multi-Target problem (multi-response least squares in Ting and Witten, 1999)
  # add features as input! (Sill, et. al, 2009)
  preds = list()
  for(i in 1:length(lrn))
    preds[[i]] = getPredictionResponse(predict(mod[[i]], task))
  target = getTaskTargetNames(task)
  newdata = data.frame(simplify2array(preds), target = getTaskTargets(task))
  colnames(newdata) = c(lrn.names, "target")
  task_sup = makeClassifTask(data = newdata, target = "target")
  lrn_sup = makeLearner("classif.ranger")
  mod_sup = train(lrn_sup, task_sup)
  mod = list(mod = mod, mod_sup = mod_sup)
  
  class(mod) = "autoMlr"
  
  return(mod)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

predict.autoMlr = function(mod, newdata, predict.type = "response") {
  pred = list()
  if(predict.type == "response") {
    for(i in 1:length(mod$mod))
      pred[[i]] = getPredictionResponse(predict(mod$mod[[i]], newdata = newdata))
    predis = data.frame(pred)
    colnames(predis) = mod$mod_sup$features
    predi = getPredictionResponse(predict(mod$mod_sup, newdata = predis))
    } else {
    for(i in 1:length(mod$mod))
      pred[[i]] = getPredictionProbabilities(predict(mod$mod[[i]], newdata = newdata))
    predis = data.frame(pred)
    colnames(predis) = mod$mod_sup$features
    predi = getPredictionResponse(predict(mod$mod_sup, newdata = predis))
  }
  factor(predi, levels = levels(pred[[1]]))
}



makeRLearner.classif.autoMlr = function() {
  makeRLearnerClassif(
    cl = "classif.autoMlr",
    package = "autoMlr",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "runtime", lower = 0)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors"), #, "prob"),
    name = "automatic choosing of models in mlr",
    short.name = "autoMlr",
    note = ""
  )
}

trainLearner.classif.autoMlr = function (.learner, .task, .subset, .weights = NULL, ...)
{
  autoMlr::autoMlr(.task, ...)
}

predictLearner.classif.autoMlr = function (.learner, .model, .newdata, ...) 
{
  predict(.model$learner.model, newdata = .newdata, predict.type = .learner$predict.type,  ...)
}


