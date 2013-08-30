\name{rxAIC}
\alias{rxAIC}
\title{Calculates Akaikes Information Criterium (AIC) from a model object.}
\usage{
  rxAIC(object, k = 2)
}
\arguments{
  \item{object}{Model object, the result of
  \code{\link[RevoScaleR]{rxLogit}},
  \code{\link[RevoScaleR]{rxLinMod}} or
  \code{\link[RevoScaleR]{rxGlm}}}

  \item{k}{Multiplier. Defaults to 2, the value for AIC}
}
\value{
  Numeric
}
\description{
  Calculates Akaikes Information Criterium (AIC) from a
  model object.
}
\examples{
library(RevoScaleR)
sampleDataDir <- rxGetOption("sampleDataDir")
working.file <- file.path(sampleDataDir, "AirlineDemoSmall.xdf")
}
\seealso{
  Other Model summary statistics: \code{\link{rxF1score}},
  \code{\link{rxLinPredError}}
}

