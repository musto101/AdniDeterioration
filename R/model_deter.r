#' modelling deterioration data
#'
#' This funcion will model the adni data, dat, which will be modelled using the
#' specified ML technique model, with hyperparameter tuning defined with values
#' found in grid.
#'
#' @param dat a dataframe.
#' @param model is a string indicating the model to be used.
#' @param grid is a matrix of hyperparam tuning values.
#' @param mcRep is a numeric value indicating the number of iterations of
#' bootstrapping are desired.
#' @param clinGroup is a string indicating the clinical group of interest.
#' @return it saves a csv of probabilities and a csv of summary statistics.
#' @export
#'
model_deter <- function(dat, model, grid, mcRep, clinGroup) {

  mcPerf <- data.frame(ROC = numeric(), Sens = numeric(), Spec = numeric(),
                       Accuracy = numeric(), Kappa = numeric())

  ctrl <- trainControl(method = 'cv', number = 5, classProbs = T,
                       summaryFunction = twoClassSummary, sampling = 'smote',
                       verboseIter = F)

  cl <- makeCluster(detectCores())
  # cl <- makeCluster(7)
  registerDoParallel(cl)

  #repeat mcRep times
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
      test <- dat[folds[[n]],]

      # # missing values imputation

      impute_train <- preProcess(training, method = "knnImpute")
      training <- predict(impute_train, training)

      impute_test <- preProcess(rbind(training[,-1], test[,-1]),
                                method = "knnImpute")

      test[,-1] <- predict(object = impute_test, test[,-1])


      booted_training <- bootstrapping(training)
      # tuning
      ml_model <- train(last_DX ~ .,
                        data = booted_training, method = model,
                        metric = "ROC",
                        #na.action = na.pass,
                        #preProcess = c("knnImpute", "scale", "center"),
                        tuneGrid = grid, trControl = ctrl)

      ### post processing cross evaluation

      # ROC
      evalResults <- data.frame(last_DX = test$last_DX)
      evalResults$rf <- predict(ml_model, test, type = "prob")[, 1]
      evalResults$newPrediction <- predict(ml_model, test)


      totalnewPrediction[folds[[n]]] <- evalResults$newPrediction
      totalprobabilities[folds[[n]]] <- evalResults$rf
    }

    v <- post_modelling_proc(dat, totalnewprediction, totalprobabilities,
                        clinGroup, mcPerf = mcPerf)

    names(v) <- c('ROC', 'Sens', 'Spec', 'Accuracy', 'Kappa')
    v <- data.frame(t(v))

    mcPerf <- rbind(mcPerf, v)
  }
  write.csv(mcPerf, paste0('data/', model, '_', clinGroup, '_',
                           '100_McPerf.csv'))

  write.csv(totalprobabilities, paste0('data/', model, '_', clinGroup, '_',
  'McPerf_probabilities.csv'))

  print('model run successfully!')
}
