#' Post modelling processing
#'
#' This function will work in the outer loop of the bootstrap and spit out
#' summary statistics for the model.
#'
#' @param dat a dataframe.
#' @param totalnewPrediction a matrix of predictions.
#' @param totalprobabilities a matrix of prediction probabilities.
#' @param clinGroup a string indicating the clinical group of interest.
#' @param mcPerf is the performance from the pervious iteration.
#' @return it returns a vector of five summary statistics.
#' @export
#'
post_modelling_proc <- function(dat, totalnewprediction, totalprobabilities,
                                clinGroup, mcPerf) {

  if (clinGroup == 'CN') {

  totalnewPrediction <- factor(totalnewPrediction, levels = c('CN', 'MCI_AD'))

  # confusion matrix all dataset
  cm <- confusionMatrix(totalnewPrediction, dat$last_DX, positive = 'MCI_AD')
  cm

  # perf
  rfROCfull <- roc(dat$last_DX, totalprobabilities, levels = c('CN', 'MCI_AD'))

  rfROC <- roc(response = dat$last_DX, totalprobabilities,
               levels = c('CN', 'MCI_AD'))

  rfThresh <- coords(rfROC, x = 'best', best.method = 'youden')

  pred <- ifelse(totalprobabilities >= rfThresh[1, 1], 'MCI_AD', 'CN')

  } else if(clinGroup == 'MCI') {

    totalnewPrediction <- factor(totalnewPrediction, levels = c('Dementia',
                                                                'CN_MCI'))
    dat$last_DX = factor(dat$last_DX, levels = c('Dementia','CN_MCI'))

    # confusion matrix all dataset
    cm <- confusionMatrix(totalnewPrediction, dat$last_DX, positive = 'Dementia')

    rfROCfull <- roc(dat$last_DX, totalprobabilities, levels = c('Dementia',
                                                                 'CN_MCI'))
    rfROC <- roc(response = dat$last_DX, totalprobabilities,
                 levels = c('Dementia', 'CN_MCI'))


    rfThresh <- coords(rfROC, x = 'best', best.method = 'youden')

    pred <- ifelse(totalprobabilities >= rfThresh[1, 1], 'CN_MCI','Dementia')

  } else {

    stop('clinGroup needs to be either CN or MCI. Please try again.')
  }

  sen <- sensitivity(factor(pred), (dat$last_DX))
  speci <- specificity(factor(pred), (dat$last_DX))
  kp <- confusionMatrix(factor(pred), (dat$last_DX))[[3]][2]
  acc <- confusionMatrix(factor(pred), (dat$last_DX))[[3]][1]

  v <- c(ROC = auc(rfROCfull), sen, speci, acc, kp)

  names(v) <- c('ROC', 'Sens', 'Spec', 'Accuracy', 'Kappa')
  v <- data.frame(t(v))

  mcPerf <- rbind(mcPerf, v)

  return(mcPerf)
}
