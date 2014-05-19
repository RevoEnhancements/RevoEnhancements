\name{rxTreeDiscretize}
\alias{rxTreeDiscretize}
\title{Function to discretize continous variables using a tree.}
\usage{
rxTreeDiscretize(formula, data, criteria = c("gini", "information"),
  subscript = "disc", sep = "_", integerLabels = FALSE,
  finalTransform = TRUE)
}
\arguments{
  \item{formula}{Formula}

  \item{data}{xdf file}

  \item{criteria}{???}

  \item{subscript}{???}

  \item{sep}{???}

  \item{integerLabels}{???}

  \item{finalTransform}{???}
}
\description{
Function to discretize continous variables using a tree.
}
\examples{
library(RevoScaleR)
claimsXdf <- file.path(rxGetOption("sampleDataDir"),"claims.xdf")
claimsXdf <- RxXdfData(claimsXdf)
discTransforms <- rxTreeDiscretize(type ~ cost + number, data = claimsXdf)
discTransforms2 <- rxTreeDiscretize(type ~ cost + number, data = claimsXdf, criteria = "information")
newData <- rxDataStep(inData = claimsXdf, transforms = discTransforms)
}
\seealso{
Other Data mining functions: \code{\link{rxDiscretize}};
  \code{\link{rxMoments}}; \code{\link{rxSample2Df}};
  \code{\link{rxUnique}}
}

