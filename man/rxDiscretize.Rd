\name{rxDiscretize}
\alias{rxDiscretize}
\title{Creates equal width or equal frequency discretized variables from continuous variables.}
\usage{
rxDiscretize(formula = ~., data, type = c("width", "freq"), nBins = NULL,
  subscript = "disc", sep = "_", integerLabels = TRUE,
  finalTransform = TRUE)
}
\arguments{
  \item{formula}{Formula}

  \item{data}{xdf file}

  \item{type}{Determines whether the discretization
  produces equal \code{width} or equal \code{freq} results}

  \item{nBins}{Number of bins}

  \item{subscript}{Character vector. Gets appended to
  column names to indicate the discretized columns.}

  \item{sep}{Separator character between column names and
  \code{subscript}}

  \item{integerLabels}{???}

  \item{finalTransform}{??? #'}
}
\description{
Creates equal width or equal frequency discretized variables from continuous variables.
}
\examples{
library(RevoScaleR)
discTransforms <- rxDiscretize(~ ., data = USArrests, integerLabels = FALSE)
discTransforms2 <- rxDiscretize(~ Assault+UrbanPop+Rape, data = USArrests, subscript = "", sep = "")

newData <- rxDataStep(inData = USArrests,
    transforms = discTransforms)
claimsXdf <- file.path(rxGetOption("sampleDataDir"),"claims.xdf")
# Equal Width
discTransforms <- rxDiscretize(~ cost, data = claimsXdf, type = "width", nBins = 1000, subscript = "disc", sep = "_")
x <- rxDataStep(inData = claimsXdf, transforms = discTransforms)

# Equal Freq
discTransforms <- rxDiscretize(~ cost, data = claimsXdf, type = "freq", nBins = 1000, subscript = "disc", sep = "_")
x <- rxDataStep(inData = claimsXdf, transforms = discTransforms)
}
\seealso{
Other Data mining functions: \code{\link{rxMoments}};
  \code{\link{rxSample2Df}};
  \code{\link{rxTreeDiscretize}}; \code{\link{rxUnique}}
}

