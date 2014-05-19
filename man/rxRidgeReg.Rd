\name{rxRidgeReg}
\alias{rxRidgeReg}
\title{Fits ridge regression model.}
\usage{
rxRidgeReg(formula, data, lambda, ...)
}
\arguments{
  \item{formula}{Model formula}

  \item{data}{Data frame or XDF}

  \item{lambda}{Regularisation coefficient.  Can be a
  single value or a vector.}

  \item{...}{Passed to \code{\link[RevoScaleR]{rxCovCor}}}
}
\description{
Fits ridge regression model using the correlation matrix.  Supports estimation of multiple values of the regularisation coefficient (lambda).
}
\author{
Derek McRae Norton
}
\references{
http://blog.revolutionanalytics.com/2014/03/extending-revoscaler-ridge-regression.html
}

