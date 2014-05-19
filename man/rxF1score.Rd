\name{rxF1score}
\alias{rxF1score}
\title{Calculates machine learning statistics, including F1, accuracy and recall.}
\usage{
rxF1score(actualVarName, predVarName, data, blocksPerRead = 1,
  reportProgress = rxGetOption("reportProgress"))
}
\arguments{
  \item{actualVarName}{A character string with the name of
  the variable containing actual (observed) binary values.}

  \item{predVarName}{A character string or vector of
  character strings with the name(s) of the variable
  containing predicted values in the [0,1] interval.}

  \item{data}{data frame, character string containing an
  '.xdf' file name (with path), or RxXdfData object
  representing an '.xdf' file containing the actual and
  observed variables.}

  \item{blocksPerRead}{number of blocks to read for each
  chunk of data read from the data source.}

  \item{reportProgress}{Passed to
  \code{\link[RevoScaleR]{rxDataStep}}}
}
\value{
A list with elements:
\describe{
\item{precision}{Precision}
\item{recall}{Recall}
\item{trueNegRate}{True negative rate}
\item{accuracy}{Accuracy}
\item{F1}{F1 score}
}
}
\description{
Calculates machine learning statistics, including F1, accuracy and recall.
}
\examples{
library(RevoScaleR)

## Demonstrates calculation on data frame

dat <- data.frame(iris[, -5], Virg = iris$Species == "virginica")
fit <- rxLogit(Virg ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
               dat)
prd <- rxPredict(fit, dat)$Virg_Pred
cmb <- data.frame(Virg=dat$Virg, Pred=prd)
rxF1score("Virg", "Pred", cmb)

## Demonstrates calculation on xdf file

dataFile <- tempfile(pattern = ".data", fileext = ".xdf")
rxDataStep(dat, outFile=dataFile, rowsPerRead=50)
rxGetInfo(dataFile)
fit <- rxLogit(Virg ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
               dataFile)
rxPredict(fit, data=dataFile, outData=dataFile)
rxGetInfo(dataFile)
rxGetVarInfo(dataFile)
rxF1score("Virg", "Virg_Pred", dataFile)
file.remove(dataFile)
}
\seealso{
Other Model summary statistics: \code{\link{rxAIC}};
  \code{\link{rxLinPredError}}
}

