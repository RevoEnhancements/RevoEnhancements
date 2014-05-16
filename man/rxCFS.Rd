\name{rxCFS}
\alias{rxCFS}
\title{Correlation feature selection.}
\usage{
rxCFS(formula, data, searchType = c("bestFirst", "exhaustive"))
}
\arguments{
  \item{formula}{Model formula}

  \item{data}{XDF}

  \item{searchType}{Either \code{bestFirst} or
  \code{Exhaustive}}
}
\description{
Correlation feature selection.
}
\examples{
form <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
rxCFS(form, data = iris)
}

