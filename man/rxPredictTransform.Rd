\name{rxPredictTransform}
\alias{rxPredictTransform}
\title{Applies a transformation on predicted values of model.}
\usage{
rxPredictTransform(modelObject, data, logModel = isLogFormula(modelObject),
  asVector = TRUE, ...)
}
\arguments{
  \item{modelObject}{object returned from a call to
  \code{rxLinMod}, \code{rxLogit}, or \code{rxGlm}. Objects
  with multiple dependent variables are not supported in
  rxPredict.}

  \item{data}{frame or XDF. New data to score.}

  \item{logModel}{Logical. If TRUE, applies exponential
  transformation, i.e. \code{exp(pred)}}

  \item{asVector}{Ignored}

  \item{...}{Passed to \code{rxPredict}}
}
\description{
The function automatically detects whether the formula in a fitted model was log transformed, and applies the inverse, i.e. \code{exp(pred)}.
}
\details{
At the moment, always returns a vector, rather than a column in a data frame or XDF.
}

