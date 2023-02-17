bootstrapping <- function(training, m, group) {
  library(dplyr)

  training <- as_tibble(training)

  x <- training %>%
    group_by(last_DX) %>%
    sample_n(replace = T, size = m)
  x <- as.data.frame(x)

  if (group == 'CN') {

    x$last_DX <-  factor(x$last_DX, levels = c('CN', 'MCI_AD'))
  } else if (group == 'MCI') {

    x$last_DX <-  factor(x$last_DX, levels = c('CN_MCI', 'Dementia'))

  } else {

    stop('ERROR: Group is not recognised')

  }

  return(x)
}
