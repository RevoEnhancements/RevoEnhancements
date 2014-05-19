\name{rxBoxPlot.stats}
\alias{rxBoxPlot.stats}
\title{Function to create a boxplot from an XDF file.}
\usage{
rxBoxPlot.stats(x, data, coef = 1.5, do.conf = TRUE, do.out = TRUE,
  blocksPerRead = rxGetOption("blocksPerRead"),
  reportProgress = rxGetOption("reportProgress"))
}
\arguments{
  \item{x}{???}

  \item{data}{xdf file}

  \item{coef}{???}

  \item{do.conf}{???}

  \item{do.out}{???}

  \item{blocksPerRead}{Passed to
  \code{\link[RevoScaleR]{rxQuantile}}}

  \item{reportProgress}{Passed to
  \code{\link[RevoScaleR]{rxQuantile}}}
}
\description{
Function to create a boxplot from an XDF file.
}
\seealso{
rxBoxPlot
}
\keyword{internal}

