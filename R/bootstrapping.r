#' Boostrapping Data
#'
#' Samples data with replacement n amount of times.With n a user defined input.
#'
#' @param dat a dataframe.
#' @param n a numeric value indicating the number of samples to take.
#' @return It returns a datafame of length n of sampled rows from the original
#' data.
#' @export
#'
bootstrapping <- function(dat, n) {

  x <- dat %>% sample_n(replace = T, size = n)

  return(x)
}