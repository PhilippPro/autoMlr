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
  lrn.name = paste0(type, ".glmnet")
  lrn[[1]] = makeLearner(cl = lrn.name, par.vals = as.list(defaults$glmnet$default[1,]))
  lrn.name = paste0(type, ".rpart")
  lrn[[2]] = makeLearner(cl = lrn.name, par.vals = as.list(defaults$rpart$default[1,]))
  lrn.name = paste0(type, ".kknn")
  lrn[[3]] = makeLearner(cl = lrn.name, par.vals = as.list(defaults$kknn$default[1,, drop = F]))
  lrn.name = paste0(type, ".liquidSVM")
  lrn[[4]] = makeLearner(cl = lrn.name)
  #lrn.name = paste0(type, ".tuneRanger")
  #lrn[[5]] = makeLearner(cl = lrn.name) #, par.vals = list(time.budget = 900))
  #lrn.name = paste0(type, ".autoxgboost")
  #lrn[[6]] = makeLearner(cl = lrn.name) #, par.vals = list(time.budget = 1800))

  # make weighted learner?
  mod = list()
  for(i in 1:length(lrn))
    mod[[i]] = train(lrn[[i]], task)
  
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
    for(i in 1:length(mod))
      pred[[i]] = getPredictionResponse(predict(mod[[i]], newdata = newdata))
    predi = apply(simplify2array(pred), 1, Mode)
    } else {
    for(i in 1:length(mod))
      pred[[i]] = getPredictionProbabilities(predict(mod[[i]], newdata = newdata))
    predi = apply(simplify2array(pred), 1:2, mean)
  }
  factor(predi, levels = levels(pred[[1]])[1:3])
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


