% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_deter.r
\name{model_deter}
\alias{model_deter}
\title{modelling deterioration data}
\usage{
model_deter(dat, model, grid, mcRep, clinGroup)
}
\arguments{
\item{dat}{a dataframe.}

\item{model}{is a string indicating the model to be used.}

\item{grid}{is a matrix of hyperparam tuning values.}

\item{mcRep}{is a numeric value indicating the number of iterations of
bootstrapping are desired.}

\item{clinGroup}{is a string indicating the clinical group of interest.}
}
\value{
it saves a csv of probabilities and a csv of summary statistics.
}
\description{
This funcion will model the adni data, dat, which will be modelled using the
specified ML technique model, with hyperparameter tuning defined with values
found in grid.
}
