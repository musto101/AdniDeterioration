#' nested cross validation
#'
#' This funcion will model the adni data in a nested cross validation procedure,
#' which will be modelled using the specified ML technique model, with
#' hyperparameter tuning defined with values found in grid.
#'
#' @param dat is a dataframe.
#' @param fold_den is the denominator for the calculation used to determine the number of folds.
#' @param fold_den is the number of iterations of a MC simulation, if desired.
#' @param grid is a grid of values for hyperparameter tuning.
#' @param ctrl is the control variables for the model.
#' @param boot is the option to conduct bootstrapping on the training data, if desired. Default is false.
#' @param m is used to determing the number of bootstaps, if using bootstrapping.
#' @param model is the desired ML model to be used.
#' @param group is the clinical group to be modelled.
#' @return it returns performance statistics for the NCV, and saves them to a csv file.
#' @export
#'
nested_cross_validation <- function(dat, folds_den, mcRep, grid, ctrl, boot = F,
                                    m = NULL, model, group) {
  library(caret)
  library(performanceEstimation)
  library(doParallel)
  library(pROC)
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)

  for (j in 1:mcRep) {
    # create nrfolds folds and start outer CV
    print(j)
    nrfolds = nrow(dat)/3

    folds <- createFolds(dat$last_DX, k = nrfolds)

    totalnewPrediction <- c(NA)
    length(totalnewPrediction) <- nrow(dat)

    totalprobabilities <- c(NA)
    length(totalprobabilities) <- nrow(dat)

    for (n in 1:nrfolds){

      training <- dat[-folds[[n]],]

      if (boot) {

      training <- bootstrapping(training = trained, m = m, group = group)

      }
      test <- dat[folds[[n]],]

      impute_train <- preProcess(training, method = "knnImpute")
      training <- predict(impute_train, training)

      impute_test <- preProcess(rbind(training[,-1], test[,-1]),
                                method = "knnImpute")

      test[,-1] <- predict(impute_test, test[,-1])

      # tuning
      model <- train(last_DX ~ ., training, method = model,
                     metric = "ROC",
                     # preProc = c("center", "scale"),
                     tuneGrid = grid,
                     trControl = ctrl)



      ### post processing cross evaluation

      # ROC
      evalResults <- data.frame(last_DX = test$last_DX)
      evalResults$rf <- predict(model, test, type = "prob")[, 1]
      evalResults$newPrediction <- predict(model, test)


      totalnewPrediction[folds[[n]]] <- evalResults$newPrediction
      totalprobabilities[folds[[n]]] <- evalResults$rf
    }
    totalnewPrediction <- ifelse(totalnewPrediction == 1, 'CN_MCI',
                                 ifelse(totalnewPrediction == 2,
                                        'Dementia', totalnewPrediction))
    totalnewPrediction <- factor(totalnewPrediction, levels = c('CN_MCI',
                                                                'Dementia'))

    # confusion matrix all dataset

    cm <- confusionMatrix(totalnewPrediction, dat$last_DX, positive = 'Dementia')
    cm

    # perf
    rfROCfull <- roc(dat$last_DX, totalprobabilities, levels = c('CN_MCI',
                                                                 'Dementia'))

    v <- c(ROC = auc(rfROCfull), cm$byClass[c(1, 2)], cm$overall[c(1, 2)])
    names(v) <- c('ROC', 'Sens', 'Spec', 'Accuracy', 'Kappa')
    v <- data.frame(t(v))

    mcPerf <- rbind(mcPerf, v)
  }

  write.csv(mcPerf, 'data/mci_gbm_boot_inner_mcperf.csv')
  return(mcPerf)

}





