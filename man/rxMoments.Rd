\name{rxMoments}
\alias{rxMoments}
\title{Extracts statistical moments and calculates skewness and kurtosis.}
\usage{
rxMoments(formula, inData = NULL, ...)
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
list with names elements for
\describe{
  \item{moments}{list of 2nd, 3rd and 4th order moments}
  \item{skewness}{list with elements g1, G1 and b1.  These correspond to the definition used for skewness in 1) traditional textbooks, 2) SAS and SPSS and 3) Minitab and BDMP}
  \item{kurtosis}{list with elements g2, G2 and b2.  These correspond to the definition used for kurtosis in 1) traditional textbooks, 2) SAS and SPSS and 3) Minitab and BDMP}
}
}
\description{
Extracts statistical moments and calculates skewness and kurtosis.
}
\examples{
library(RevoScaleR)
if(require("e1071")){
  skewness(iris$Sepal.Length, type=3)
  skewness(iris$Sepal.Width, type=1)
  kurtosis(iris$Sepal.Length, type=3)
  kurtosis(iris$Sepal.Width, type=1)
}
rxMoments(~ Sepal.Length + Sepal.Width, iris)
rxMoments(~ Sepal.Width + Sepal.Length, iris)
}
\seealso{
\code{\link[RevoScaleR]{rxSummary}}

Other Data mining functions: \code{\link{rxDiscretize}};
  \code{\link{rxSample2Df}};
  \code{\link{rxTreeDiscretize}}; \code{\link{rxUnique}}
}

