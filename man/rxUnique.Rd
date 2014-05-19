\name{rxUnique}
\alias{rxUnique}
\title{Extracts unique values from character and factor columns in xdf.}
\usage{
rxUnique(formula, inData = NULL, ...)
}
\arguments{
  \item{formula}{formula, as described in
  \code{\link[RevoScaleR]{rxFormula}}. The formula
  typically does not contain a response variable, i.e. it
  should be of the form ~ terms.}

  \item{inData}{either a data source object, a character
  string specifying a '.xdf' file, or a data frame object
  to summarize.}

  \item{...}{Other arguments passed to
  \code{\link[RevoScaleR]{rxDataStep}}}
}
\value{
list with element for each named row in the formula
}
\description{
Extracts unique values from character and factor columns in xdf.
}
\examples{
library(RevoScaleR)
dataSource <- file.path(rxGetOption("sampleDataDir"), "CensusWorkers.xdf")
rxGetVarInfo(dataSource)
rxUnique(~sex, inData=dataSource)
rxUnique(~sex+state, inData=dataSource)
rxUnique(~state+sex, inData=dataSource)
}
\seealso{
Other Data mining functions: \code{\link{rxDiscretize}};
  \code{\link{rxMoments}}; \code{\link{rxSample2Df}};
  \code{\link{rxTreeDiscretize}}
}

