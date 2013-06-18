#
#  RevoEnhancements/R/sample.R by Derek Norton andAndrie de Vries  Copyright (C) 2012-2013
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

#' Function to sample, with or without replacement, from an XDF to a data frame.
#' 
#' @param data xdf File
#' @param size ???
#' @param replace ???
#' @export
#' @family Data mining functions
#' @examples
#' library(RevoScaleR)
#' xdfFile <- file.path(rxGetOption("sampleDataDir"), "CensusWorkers.xdf")
#' df <- rxSample2Df(xdfFile , size = 10000, replace = FALSE)
#' df <- rxSample2Df(xdfFile , size = 10000, replace = TRUE)
rxSample2Df <- function(data, size, replace = FALSE) {
  extraRows <- 100
  dataInfo <- rxGetInfo(data, getVarInfo = TRUE)
  oneVar <- names(dataInfo$varInfo[1])
  dataSize <- dataInfo$numRows
  if (size > 1) {
    p <- size / dataSize 
  } else if (size > 0) {
    size <- ceiling(size * dataSize)
    p <- size / dataSize
  } else {
    stop("'size' must be greater than zero")
  }
  if (is.data.frame(data)) {
    mySamp <- sample.int(n = dataSize, size = size, replace = ifelse(size > dataSize, TRUE, replace))
    return(data[mySamp,])
  }
  createRandomSample <- function(dataList) {
    numRows <- length(dataList[[1]])
    dataList$.rxRowSelection <- as.logical(rbinom(numRows,1, zP))
    return(dataList)
  }
  createRandomSampleReplace <- function(dataList) {
    numRows <- length(dataList[[1]])
    rowNum <- seq_len(numRows) + .rxStartRow - 1 
    rows <- sample[sample %in% rowNum]
    tmpDf <- as.data.frame(dataList)[rows - .rxStartRow + 1,]
    row.names(tmpDf) <- as.numeric(row.names(tmpDf)) + .rxStartRow - 1 
    ret[[.rxChunkNum]] <- tmpDf
    .rxSet("ret", ret)
    return(NULL)
  }
  if (replace) {
    mySamp <- sample.int(dataSize, size = size, replace = replace)
    ret <- rxDataStep(data, transformFunc = createRandomSampleReplace,
        transformObjects = list(sample = mySamp, ret = list()),
        returnTransformObjects = TRUE)
    ret <- do.call(rbind, ret$ret)
  } else {
    newP <- p + extraRows / dataSize
    ret <- head(rxDataStep(data, 
        transformFunc = createRandomSample,
        transformVars = oneVar, 
        transformObjects = list(zP = newP)), 
      n = size)
  }
  return(ret)
}

