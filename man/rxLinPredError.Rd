\name{rxLinPredError}
\alias{rxLinPredError}
\title{Calculates prediction error statistics for linear regression models.}
\usage{
rxLinPredError(actualVarName, predVarName, data, sWeights = NULL,
  blocksPerRead = 1, reportProgress = rxGetOption("reportProgress"))
}
\arguments{
  \item{actualVarName}{String name of the response
  variable.}

  \item{predVarName}{String name of the predicted
  variable.}

  \item{sWeights}{String name of error weights.}

  \item{data}{data frame, or character string containing an
  '.xdf' file name (with path), or RxXdfData object
  representing an '.xdf' file containing the actual and
  observed variables.}

  \item{blocksPerRead}{number of blocks to read for each
  chunk of data read from the data source.}

  \item{reportProgress}{Passed to
  \code{\link[RevoScaleR]{rxDataStep}}}
}
\value{
returns a list of prediction measures MSE, MAPE, MPE, MSWD
}
\description{
Calculates a number of error statistics, including:
\describe{
\item{MSE}{Mean squared error}
\item{MAPE}{Mean absolute percentage erorr}
\item{MPE}{Mean percentage error}
\item{MSWD}{Mean squared weighted deviation}
}
}
\examples{
library(RevoScaleR)

## Demonstrates calculation on data frame

fit <- rxLinMod(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
prd <- rxPredict(fit, iris)$Sepal.Length_Pred
dat <- data.frame(Sepal.Length=iris$Sepal.Length, Sepal.Length_Pred=prd, Weights = rep(1, nrow(iris))/nrow(iris))
rxLinPredError("Sepal.Length", "Sepal.Length_Pred", data=dat, sWeights="Weights")
rxLinPredError("Sepal.Length", "Sepal.Length_Pred", data=dat)
}
\seealso{
Other Model summary statistics: \code{\link{rxAIC}};
  \code{\link{rxF1score}}
}

