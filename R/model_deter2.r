model_deter2 <- function(training, test, grid, model) {
  library(performanceEstimation)
  library(doParallel)

  ctrl <- trainControl(method = 'cv', number = 5, classProbs = T,
                       summaryFunction = twoClassSummary, sampling = 'smote',
                       verboseIter = F)
#
#   cl <- makeCluster(detectCores())
#   # cl <- makeCluster(7)
#   registerDoParallel(cl)

  # impute_train <- preProcess(training, method = "knnImpute")
  # training <- predict(impute_train, training)
  #
  # impute_test <- preProcess(rbind(training[,-1], test[,-1]),
  #                           method = "knnImpute")
  #
  # test[,-1] <- predict(impute_test, test[,-1])
  # # training2 <- training
  # # test2 <- test
  #
  # training$last_DX <- as.factor(training$last_DX)
  # training$last_DX <- factor(training$last_DX,
  #                            levels = rev(levels(training$last_DX)))
  # test$last_DX <- as.factor(test$last_DX)
  # test$last_DX <- factor(test$last_DX,
  #                        levels = rev(levels(test$last_DX)))

  ml_model <- train(last_DX ~ .,
                    data = training, method = model,
                    metric = "ROC",
                    #na.action = na.pass,
                    #preProcess = c("knnImpute", "scale", "center"),
                    tuneGrid = grid, trControl = ctrl)

  predictions <- predict(ml_model, test, type = "prob")[, 1]
  hard_pred <- predict(ml_model, test)

  preds <- list(predictions, hard_pred)

  return(preds)



}
