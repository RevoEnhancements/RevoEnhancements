# Function to sample, with or without replacement, from an XDF to a data frame.

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

## Examples
# xdfFile <- file.path(rxGetOption("sampleDataDir"), "CensusWorkers.xdf")
# df <- rxSample2Df(xdfFile , size = 10000, replace = FALSE)
# df <- rxSample2Df(xdfFile , size = 10000, replace = TRUE)
