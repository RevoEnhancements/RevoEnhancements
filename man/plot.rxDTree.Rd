\name{plot.rxDTree}
\alias{plot.rxDTree}
\title{Plots rxDTree object.}
\usage{
\method{plot}{rxDTree}(x, text = TRUE, plotArgs = NULL, textArgs = NULL,
  ...)
}
\arguments{
  \item{x}{rxDTree object}

  \item{text}{If TRUE, adds text labels}

  \item{plotArgs}{Named list, passed to
  \code{\link[rpart]{plot.rpart}}}

  \item{textArgs}{Names list, passed to
  \code{\link[rpart]{text.rpart}}}

  \item{...}{Not used. Required for consistency with other
  plot methods.}
}
\description{
Plots rxDTree object.
}
\examples{
### rxDTree examples
library("RevoScaleR")
library("ggplot2")
frm <- formulaExpand(price ~ ., data=diamonds)
fit <- rxDTree(frm, diamonds, maxDepth=3)
par(mar=c(0.5, 0.5, 2, 0.5))
plot(fit)
plot(fit, textArgs=list(col="blue", cex=0.7))
plot(fit, textArgs=list(col="blue", cex=0.7), plotArgs=list(main="Forest 1"))


if(exists("rxDForest")){
  fit <- rxDForest(frm, diamonds, maxDepth=3)
  forest <- fit$forest
  par(mar=c(0.5, 0.5, 2, 0.5))
  plot(forest[[1]])
  plot(forest[[1]], textArgs=list(col="blue", cex=0.7))
  plot(forest[[1]], textArgs=list(col="blue", cex=0.7), plotArgs=list(main="Forest 1"))
  plot(forest[[2]], textArgs=list(col="blue", cex=0.7), plotArgs=list(main="Forest 2"))
}
}

