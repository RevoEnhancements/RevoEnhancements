\name{rxHexBin}
\alias{rxHexBin}
\title{Creates hexbin summaries.}
\usage{
rxHexBin(formula, data, shape = 1, xbins = 30, ...)
}
\arguments{
  \item{formula}{formula}

  \item{data}{xdf file}

  \item{shape}{shape parameter, passed to
  \code{\link[hexbin]{hexbin}}}

  \item{xbins}{Number of xbins, passed to
  \code{\link[hexbin]{hexbin}}}

  \item{...}{Other arguments, passed to
  \code{\link[RevoScaleR]{rxDataStep}}}
}
\description{
Creates hexbin summaries.
}
\examples{
# Load the Airlines Data
library(RevoScaleR)
working.file <- file.path(rxGetOption("sampleDataDir"), "AirlineDemoSmall.xdf")

library(RevoScaleR)
library(hexbin)
bin1 <- rxHexBin(ArrDelay ~ CRSDepTime, working.file, xbins = 50)
plot(bin1, lcex=.5, border = TRUE, colramp = BTC)
plot(bin1, style="colorscale")
plot(bin1, style="centroids")
plot(bin1, style="lattice")

library(ggplot2)
ggbin <- data.frame(
  hcell2xy(bin1),
  count = bin1@count,
  density = bin1@count / sum(bin1@count, na.rm=TRUE)
)
p2 <- ggplot(data = ggbin, aes(x, y, fill = count))
p2 + geom_hex(stat="identity", nbins = 100) +
  guides(fill = guide_colorbar(ticks = FALSE, label.theme = element_text(size = 8, angle = 0))) +
  labs(x = "CRSDepTime", y = "ArrDelay")
}

