\name{rxAIC}
\alias{rxAIC}
\title{Calculates Akaike's Information Criterium (AIC) from a model object.}
\usage{
rxAIC(object, k = 2)
}
\arguments{
  \item{object}{Model object, the result of
  \code{\link[RevoScaleR]{rxLogit}},
  \code{\link[RevoScaleR]{rxLinMod}} or
  \code{\link[RevoScaleR]{rxGlm}}}

  \item{k}{numeric, the penalty per parameter to be used;
  the default k = 2 is the classical AIC}
}
\value{
Numeric
}
\description{
Calculates Akaike's Information Criterium (AIC) from a model object.
}
\examples{
library(RevoScaleR)
sampleDataDir <- rxGetOption("sampleDataDir")
airline <- file.path(sampleDataDir, "AirlineDemoSmall.xdf")
frm <- formulaExpand(ArrDelay ~ ., airline)
model <- rxLinMod(frm, airline)
rxAIC(model)
}
\seealso{
\code{\link[stats]{AIC}}, \code{\link[stats]{BIC}}

Other Model summary statistics: \code{\link{rxF1score}};
  \code{\link{rxLinPredError}}
}

