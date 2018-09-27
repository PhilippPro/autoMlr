library(h2o)

h2o.init()

# Import a sample binary outcome train/test set into H2O
train <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_train_10k.csv")
test <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_test_5k.csv")

# Identify predictors and response
y <- "response"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

aml <- h2o.automl(x = x, y = y,
  training_frame = train,
  leaderboard_frame = test,
  max_runtime_secs = 30)

# View the AutoML Leaderboard
lb <- aml@leaderboard
lb

#                                             model_id      auc  logloss
# 1           StackedEnsemble_model_1494643945817_1709 0.780384 0.561501
# 2 GBM_grid__95ebce3d26cd9d3997a3149454984550_model_0 0.764791 0.664823
# 3 GBM_grid__95ebce3d26cd9d3997a3149454984550_model_2 0.758109 0.593887
# 4                          DRF_model_1494643945817_3 0.736786 0.614430
# 5                        XRT_model_1494643945817_461 0.735946 0.602142
# 6 GBM_grid__95ebce3d26cd9d3997a3149454984550_model_3 0.729492 0.667036
# 7 GBM_grid__95ebce3d26cd9d3997a3149454984550_model_1 0.727456 0.675624
# 8 GLM_grid__95ebce3d26cd9d3997a3149454984550_model_1 0.685216 0.635137
# 9 GLM_grid__95ebce3d26cd9d3997a3149454984550_model_0 0.685216 0.635137

# The leader model is stored here
aml@leader


# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

#pred <- h2o.predict(aml, test)  #Not functional yet: https://0xdata.atlassian.net/browse/PUBDEV-4428

# or:
pred <- h2o.predict(aml@leader, test)