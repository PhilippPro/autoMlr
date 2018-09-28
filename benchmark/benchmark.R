# compare with: 
# ranger
# liquidSVM
# autoxgboost
# tuneRanger
# h2o
# SuperLearner

library(devtools)
library(OpenML)
library(mlr)
load_all("../autoMlr")


lrns = list(
  makeLearner("classif.autoMlr"),
  makeLearner("classif.ranger")
)

rdesc = makeResampleDesc("Holdout")
configureMlr(on.learner.error = "warn")
measures = list(mmce, timetrain) #list(mmce, multiclass.au1p, multiclass.brier, logloss, timetrain)
configureMlr(on.learner.error = "warn")
set.seed(126)
mod = train(lrns[[1]], iris.task)
predict(mod, iris.task)

bmr1 = benchmark(lrns, iris.task, rdesc, measures)
bmr1

library(OpenML)
#task.ids = listOMLTasks(number.of.classes = 2L, number.of.missing.values = 0, tag = "OpenML100", estimation.procedure = "10-fold Crossvalidation")$task.id
load(file = "./benchmark/task_ids.RData")

# time estimation
load("./benchmark/time.estimate.RData")

rdesc = makeResampleDesc("RepCV", reps = 10, folds = 5)
# benchmark
bmr_liquid = list()

# Choose small and big datasets
# select datasets where RF do not take longer than ...

# take only small ones first; afterwards some bigger datasets
task.ids.bmr = task.ids[which((unlist(time.estimate)-100)<60)]
cbind(time.estimate, (unlist(time.estimate)-100)<60)
unlist(time.estimate)[which((unlist(time.estimate)-100)<60)]

for(i in seq_along(task.ids.bmr)) { # 13 datasets
  print(i)
  set.seed(200 + i)
  task = getOMLTask(task.ids.bmr[i])
  task = convertOMLTaskToMlr(task)$mlr.task
  bmr_liquid[[i]] = benchmark(lrns, task, rdesc, measures, keep.pred = FALSE, models = FALSE)
  save(bmr, file = "./benchmark/bmr.RData")
}
load("./benchmark/bmr.RData")