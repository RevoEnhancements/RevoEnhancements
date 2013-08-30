#
# RevoEnhancements/R/rxSample.R by Derek Norton and Andrie de Vries
#
# Copyright 2013 Revolution Analytics
#    
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#      http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Function to sample, with or without replacement, from an XDF to a data frame.
#' 
#' @param data xdf File
#' @param size Number of rows to sample
#' @param replace If TRUE, samples with replacement.  Passed to \code{\link{sample.int}}
#' @param maxRowsByCols the maximum number of rows x columns as specified by rxDataStep
#' @export
#' @return A data frame
#' @family Data mining functions
#' @examples
#' library(RevoScaleR)
#' xdfFile <- file.path(rxGetOption("sampleDataDir"), "CensusWorkers.xdf")
#' df <- rxSample2Df(xdfFile, size = 10000, replace = FALSE)
#' df <- rxSample2Df(xdfFile, size = 10000, replace = TRUE)
rxSample2Df <- function(data, size, replace = FALSE, maxRowsByCols = 3E6) {
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
    mySamp <- sample.int(n = dataSize, size = size, 
                         replace = ifelse(size > dataSize, TRUE, replace))
    return(data[mySamp,])
  }
  createRandomSample <- function(dataList) {
    # Trick to pass R CMD check: create and remove variables without binding
    .rxStartRow <- .rxChunkNum <- function(){}
    rm(.rxStartRow, .rxChunkNum)
    zP <- character()
    rm(zP)
    
    numRows <- length(dataList[[1]])
    dataList$.rxRowSelection <- as.logical(rbinom(numRows,1, zP))
    return(dataList)
  }
  createRandomSampleReplace <- function(dataList) {
    # Trick to pass R CMD check: create and remove variables without binding
    .rxGet <- .rxSet <- .rxStartRow <- .rxChunkNum <- function() {}
    rm(.rxGet, .rxSet, .rxStartRow, .rxChunkNum)
    
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
    ret <- rxDataStep(data, 
                      transformFunc = createRandomSampleReplace,
                      transformObjects = list(sample = mySamp, ret = list()),
                      returnTransformObjects = TRUE, maxRowsByCols = maxRowsByCols)
    ret <- do.call(rbind, ret$ret)
  } else {
    newP <- p + extraRows / dataSize
    ret <- head(rxDataStep(data, 
                           transformFunc = createRandomSample,
                           transformVars = oneVar, 
                           transformObjects = list(zP = newP),
                           maxRowsByCols = maxRowsByCols), 
                n = size)
  }
  return(ret)
}

