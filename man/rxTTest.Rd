\name{rxTTest}
\alias{rxTTest}
\title{Function to carry out a student's t test on an xdf file}
\usage{
rxTTest(formula, data, alternative = c("two.sided", "less", "greater"),
  mu = 0, var.equal = FALSE, conf.level = 0.95, ...)
}
\arguments{
  \item{formula}{???}

  \item{data}{???}

  \item{alternative}{???}

  \item{mu}{???}

  \item{var.equal}{???}

  \item{conf.level}{???}

  \item{...}{???}
}
\description{
Function to carry out a student's t test on an xdf file
}
\examples{
# Generate Data and test differences
library(RevoScaleR)
xdfFile <- "myTTestFile.xdf"
x <- data.frame(x = rnorm(1000, mean = -1, sd = 1), y = rnorm(1000, mean = 6, sd = 3), group = factor(rbinom(1000, 1, .6)))
rxDataStep(inData = x, outFile = xdfFile, overwrite = TRUE)

# One-Sample t-test
t.test(x$x)
rxTTest(~ x, data = xdfFile)

# Two-Sample t-test
t.test(x$x, x$y)
rxTTest(~ x + y, data = xdfFile)

# Grouped Two-Sample t-test
t.test(x ~ group, data = x)
rxTTest(x ~ group, data = xdfFile)

# Paired t-test
t.test(x$x, x$y, paired = TRUE)
rxTTest(~ I(x - y), data = xdfFile)

# Grouped Two-Sample Paired t-test
# Not available in t.test()
rxTTest(I(x - y) ~ group, data = xdfFile)
}
\seealso{
Other Modelling functions: \code{\link{rxKS.rxLorenz}}
}

