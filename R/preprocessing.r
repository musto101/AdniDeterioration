#' preprocessing from adni_slim data
#'
#' preprocesses the adni_slim data by removing columns with perc missing values
#' and filters on the relevant clinical group clinGroup. Also dummies and records
#' location of missing values.
#'
#' @param dat is a dataframe
#' @param perc is the percentage of missing values, where if
#' column missing > perc, column is removed.
#' @param clinGroup is the clinical group desired to filter on.
#' @return It returns a dataframe of the preprocessed data
#' @export
#'
preprocessing <- function(dat, perc, clinGroup) {

  missing.perc <- apply(dat, 2, function(x) sum(is.na(x))) / nrow(dat)

  dat <- dat[, which(missing.perc < perc)]
  dat <- dat[dat$PTMARRY != 'Unknown',]
  dat <- dat[dat$last_visit > 0,]

  y <- dat %>%
    mutate_all(funs(ifelse(is.na(.), 1, 0)))

  blah <- y %>%
    select_if(function(col) is.numeric(col) && sum(col) == 0) # sanity check

  y <- y %>%
    select_if(negate(function(col) is.numeric(col) && sum(col) == 0))

  names(y) <- paste0(names(y), '_na')

  dat <- cbind(dat, y)

  dummies <- dummyVars(last_DX ~., data = dat)
  data_numeric <- predict(dummies, newdata= dat)
  data_numeric <- as.data.frame(data_numeric)
  data_numeric <-data.frame(dat$last_DX, data_numeric)
  names(data_numeric)[1] <- 'last_DX'
  data_numeric$X <- NULL

  if (clinGroup == 'CN') {

  cn_progress <- data_numeric[data_numeric$DXCN == 1,]
  cn_progress$last_DX <- factor(ifelse(cn_progress$last_DX == 'CN',
                                       'CN', 'MCI_AD'),
                                levels = c('CN', 'MCI_AD'))

  } else if (clinGroup == 'MCI') {

    cn_progress <- data_numeric[data_numeric$DXMCI == 1,]
    cn_progress$last_DX <- factor(ifelse(cn_progress$last_DX == 'Dementia',
                                         'Dementia', 'CN_MCI'),
                                  levels = c('CN_MCI', 'Dementia'))



  } else {
    stop('clinGroup needs to be either CN or MCI. Please try again.')
  }

  cn_progress$DXCN <- NULL
  cn_progress$DXDementia <- NULL
  cn_progress$DXMCI <- NULL

  return(cn_progress)
}
