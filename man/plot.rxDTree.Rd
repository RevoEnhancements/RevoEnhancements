\name{plot.rxDTree}
\alias{plot.rxDTree}
\title{Plots rxDTree object.}
\usage{
  \method{plot}{rxDTree} (x, text = TRUE, plotArgs, ...)
}
\arguments{
  \item{x}{rxDTree object}

  \item{text}{If TRUE, adds text labels}

  \item{plotArgs}{Named list, passed to
  \code{link[rpart]{plot.rpart}}}

  \item{...}{Other arguments, passed to
  \code{link[rpart]{text.rpart}}}
}
\description{
  Plots rxDTree object.
}
\examples{
library("RevoScaleR")
frm <- as.formula(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width)

fit <- rxDTree(frm, data = iris)
plot(fit)

if(exists("rxDForest")){
  fit <- rxDForest(frm, data = iris)
  plot(fit$forest[[1]])
  plot(fit$forest[[3]])
}
}

