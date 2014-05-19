\docType{package}
\name{RevoEnhancements-package}
\alias{RevoEnhancements}
\alias{RevoEnhancements-package}
\title{Adds functionality to RevoScaleR}
\description{
This package adds functionality to RevoScaleR. All functions require, and are enhancements to, [Revolution R Enterprise](http://www.revolutionanalytics.com/products/revolution-enterprise.php).
}
\details{
This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 or 3 of the License (at your option).

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

A copy of the GNU General Public License is available at http://www.r-project.org/Licenses/

Areas of enhancement are:

\itemize{
\item{CRAN R Compatability}
\item{Big Data Graphics}
\item{Big Data Mining}
\item{Discretization}
\item{Variable Importance / Selection}
\item{Models}
\item{Scoring}
\item{...}
}

Some functions you might find useful:

Compatibility and utility functions:
\itemize{
\item{\code{\link{formulaExpand}}}
\item{\code{\link{range2formula}}}
}

Modeling:
\itemize{
\item{\code{\link{rxKS}}}
\item{\code{\link{rxTTest}}}
\item{\code{\link{rxMoments}}}
}

Data visualisation:
\itemize{
\item{\code{\link{rxBoxPlot}}}
\item{\code{\link{rxHexBin}}}
}

Data mining:
\itemize{
\item{\code{\link{rxUnique}}} to retrieve unique values in columns
\item{\code{\link{rxDiscretize}}} to discretize (bin) values
\item{\code{\link{rxSample2Df}}} to sample values to a data frame
\item{\code{\link{rxTreeDiscretize}}} to discretize a tree object
\item{\code{\link{rxF1score}}} to calculate model precision, sensitivity and F1 score
\item{\code{\link{rxLinPredError}}} to calculate prediction error for linear models
}
}
\author{
Derek Norton \email{derek.norton@revolutionanalytics.com} and Andrie de Vries \email{andrie@revolutionanalytics.com}
}
\keyword{package}

