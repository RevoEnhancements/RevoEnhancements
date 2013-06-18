#
#  RevoEnhancements/R/rxBoxPlot.R by Derek Norton andAndrie de Vries  Copyright (C) 2012-2013
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#


#' Function to create a boxplot from an XDF file.
#' 
#' @param x ???
#' @param data xdf file
#' @param coef ???
#' @param do.conf ???
#' @param do.out ???
#' @param blocksPerRead Passed to \code{\link[RevoScaleR]{rxQuantile}}
#' @param reportProgress Passed to \code{\link[RevoScaleR]{rxQuantile}}
#' @keywords internal
#' @seealso rxBoxPlot
rxBoxPlot.stats <- function(x, data, coef = 1.5, do.conf = TRUE, do.out = TRUE,
              blocksPerRead = rxGetOption("blocksPerRead"),
              reportProgress = rxGetOption("reportProgress")){
  if (coef < 0) 
      stop("'coef' must not be negative")
  outputFile <- file.path(getwd(), "BoxPlotStatsTemp.xdf")
  workData <- rxDataStep(inData = data, outFile = outputFile,
    varsToKeep = x, removeMissings = TRUE,
    overwrite = TRUE)
  n <- rxGetInfo(workData)$numRows
  stats <- rxQuantile(x, data, probs = seq(0, 1, 0.25),
    blocksPerRead = blocksPerRead, reportProgress = reportProgress, numericBins = FALSE)
  iqr <- diff(stats[c(2, 4)])
  if (coef == 0) {
    do.out <- FALSE
  } else {
    rowSel <- parse(text = paste(x, "< lw |", x, "> uw"))
    out <- rxDataStep(inData = workData, 
      rowSelection = rowSel,
      transformObjects = list(lw = stats[2L] - coef * iqr,
        uw = stats[4L] + coef * iqr)
      )[,1]
    stats[c(1, 5)] <- c(stats[2L] - coef * iqr, stats[4L] + coef * iqr)
    ## This is what it should be: range(x[!out], na.rm = TRUE)
  }
  conf <- if (do.conf) {
    stats[3L] + c(-1.58, 1.58) * iqr/sqrt(n)
  }
  unlink(outputFile)
  list(stats = stats, n = n, conf = conf, out = if (do.out) out else numeric())
}

#' Function to create a boxplot from an XDF file.
#' 
#' @param formula ???
#' @param data xdf file
#' @param rowSelection ???
#' @param blocksPerRead ???
#' @param title ???
#' @param subtitle ???
#' @param xTitle ???
#' @param yTitle ???
#' @param boxFillColor ???
#' @param boxOutlineColor ???
#' @param outlierColor ???
#' @param outlierStyle ???
#' @param outlierSize ???
#' @param outlierBackground ???
#' @param plotAreaColor ???
#' @param gridColor ???
#' @param gridLineWidth ???
#' @param gridLineStyle ???
#' @param whiskerLineType ???
#' @param whiskerLineWidth ???
#' @param whiskerLineColor ???
#' @param medianLineType ???
#' @param medianLineWidth ???
#' @param medianLineColor ???
#' @param reportProgress ???
#' @export
#' @family plot functions
rxBoxPlot <- function(formula, data, rowSelection = NULL, 
              blocksPerRead = rxGetOption("blocksPerRead"),
              title = NULL, subtitle = NULL, xTitle = NULL, yTitle = NULL,
              boxFillColor = "skyblue", boxOutlineColor = "black",
              outlierColor = NULL, outlierStyle = 1, outlierSize = 1,
              outlierBackground = NULL, plotAreaColor = "gray90", gridColor = "white", 
              gridLineWidth = 1, gridLineStyle = "solid", whiskerLineType= "solid", 
              whiskerLineWidth = 1, whiskerLineColor = "black", medianLineType= "solid", 
              medianLineWidth = 1, medianLineColor = "black", 
              reportProgress = rxGetOption("reportProgress")){
  if (missing(formula)) 
    stop("'formula' missing")
  tf <- terms(formula)
  vars <- all.vars(formula)
  outputFile <- file.path(getwd(), "boxplotTemp.xdf")
  workData <- rxDataStep(inData = data, outFile = outputFile,
    varsToKeep = vars, rowSelection = rowSelection,
    overwrite = TRUE)
  
  boxList <- rxBoxPlot.stats(vars[1], workData)
  unlink(outputFile)
  dim(boxList$stats) <- c(5,1)
	boxList$group <- as.numeric(is.numeric(boxList$out))
  if (TRUE) {
	  boxList$names <- vars[1]
    do.call("bxp", list(boxList, show.names = TRUE, 
      main = title, sub = subtitle, xlab = xTitle, ylab = yTitle))
    rect(par("usr")[1], par("usr")[3], par("usr")[2], 
      par("usr")[4], col = plotAreaColor)
    grid(nx=length(vars)+2, ny=NULL, col = gridColor, 
      lty = gridLineStyle, lwd = gridLineWidth)
    do.call("bxp", list(boxList, 
      boxfill = boxFillColor, boxcol = boxOutlineColor, 
      whisklty = whiskerLineType, whisklwd = whiskerLineWidth, 
      whiskcol = whiskerLineColor, medlty = medianLineType, 
      medlwd = medianLineWidth, medcol = medianLineColor,
      outbg = outlierBackground, outcol = outlierColor, outpch = outlierStyle,
      outcex = outlierSize, add = TRUE))
	  invisible(boxList)
  } else {
    boxList
  }
}

## Need to add grouping

## Examples
#claimsXdf <- file.path(rxGetOption("sampleDataDir"),"claims.xdf")
#claimsXdf <- RxXdfData(claimsXdf)
#x <- rxXdfToDataFrame(file = claimsXdf)
#a1 <- boxplot.stats(x$cost)
#b1 <- rxBoxPlot.stats("cost", claimsXdf)
#all.equal(a1,b1) ## Won't be because rxQuantiles are not exact

#a2 <- boxplot(x$cost)
#b2 <- rxBoxPlot(~ cost, claimsXdf, title = "My Test Boxplot", subtitle = "Groups are not added yet")