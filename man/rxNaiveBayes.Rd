\name{rxNaiveBayes}
\alias{rxNaiveBayes}
\title{Naive Bayes.}
\usage{
rxNaiveBayes(formula, data, laplace = 0, ...)
}
\arguments{
  \item{formula}{formula}

  \item{data}{data frame or XDF}

  \item{laplace}{positive double controlling Laplace
  smoothing. The default (0) disables Laplace smoothing}

  \item{...}{not used}
}
\description{
Implementation of naive bayes using package e1071.
}
\details{
Naive Bayes is a simple probabilistic classifier based on applying Bayes' theorem with strong (naive) independence assumptions. This is often a good benchmark for other more complicated data mining models.

Really you are just calculating proportions for categorical variables (with possible Laplace correction), and probabilities based on a normal distribution for numeric variables. The proportions are easily calculated using rxCrossTabs, and the normal probabilities are easily calculated given a mean and standard deviation which we can get from rxSummary.

We can use existing \code{e1071} code and replace the calculation of proportions and probabilities with big data versions. The results are not only not big data, but existing methods work on object!
}
\examples{
data(HouseVotes84, package = "mlbench")
hv <- HouseVotes84
hv$V17 <- rnorm(nrow(hv), mean = c(-3, 5)[as.numeric(hv$Class)], sd = c(.5, 2)[as.numeric(hv$Class)])
hv$V18 <- rnorm(nrow(hv), mean = c(2, 15)[as.numeric(hv$Class)], sd = c(4, 1)[as.numeric(hv$Class)])
model1 <- naiveBayes(Class ~ ., data = hv)
model2 <- rxNaiveBayes(Class ~ ., data = hv)
summary(model1)
summary(model2)
}

