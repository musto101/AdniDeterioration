data_split <- function(dat, y, size) {

  library(caret)
  library(ROSE)

  dat_part <- createDataPartition(y = dat[,y], times = 1, p = size, list = F)

  training <- dat[dat_part, ]
  test <- dat[-dat_part, ]

  # training_rose <- ROSE(last_DX ~., training, seed = 1)$data

  impute_train <- preProcess(training, method = "knnImpute")
  training <- predict(impute_train, training)

  impute_test <- preProcess(rbind(training[,-1], test[,-1]),
                            method = "knnImpute")

  test[,-1] <- predict(impute_test, test[,-1])
  # training2 <- training
  # test2 <- test

  # training$last_DX <- as.factor(training$last_DX)
  # training$last_DX <- factor(training$last_DX,
  #                            levels = rev(levels(training$last_DX)))
  # test$last_DX <- as.factor(test$last_DX)
  # test$last_DX <- factor(test$last_DX,
  #                        levels = rev(levels(test$last_DX)))

  # X_train <- training[, -y]
  # y_train <- training[, y]
  #
  # X_train <- training[, -y]
  # y_train <- training[, y]

  #training_rose <- ROSE(last_DX ~., training, seed = 1)$data

  table(training$last_DX)

  return(list(training, test))
}

