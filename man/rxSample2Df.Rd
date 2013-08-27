\name{rxSample2Df}
\alias{rxSample2Df}
\title{Function to sample, with or without replacement, from an XDF to a data frame.}
\usage{
  rxSample2Df(data, size, replace = FALSE)
}
\arguments{
  \item{data}{xdf File}

  \item{size}{Number of rows to sample}

  \item{replace}{If TRUE, samples with replacement.  Passed
  to \code{\link{sample.int}}}
}
\value{
  A data frame
}
\description{
  Function to sample, with or without replacement, from an
  XDF to a data frame.
}
\examples{
library(RevoScaleR)
xdfFile <- file.path(rxGetOption("sampleDataDir"), "CensusWorkers.xdf")
df <- rxSample2Df(xdfFile, size = 10000, replace = FALSE)
df <- rxSample2Df(xdfFile, size = 10000, replace = TRUE)
}
\seealso{
  Other Data mining functions: \code{\link{rxDiscretize}},
  \code{\link{rxTreeDiscretize}}
}

