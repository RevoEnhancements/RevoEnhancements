#
#  RevoEnhancements/R/discretize.R by Derek Norton and Andrie de Vries  Copyright (C) 2012-2013
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


#' Function to create equal width or equal frequency discretized variables from continuous variables.
#'
#' @param formula Formula
#' @param data xdf file
#' @param type Character vector, either "width" or "freq"
#' @param nBins ???
#' @param subscript ???
#' @param sep ???
#' @param integerLabels ???
#' @param finalTransform ???
#' #' @export
#' @family Data mining functions
#' @examples
#' library(RevoScaleR)
#' discTransforms <- rxDiscretize(~ ., data = USArrests, integerLabels = FALSE)
#' discTransforms2 <- rxDiscretize(~ Assault+UrbanPop+Rape, data = USArrests, subscript = "", sep = "")
#'
#' newData <- rxDataStep(inData = USArrests, 
#'     transforms = discTransforms)
#' claimsXdf <- file.path(rxGetOption("sampleDataDir"),"claims.xdf")

#' # Equal Width
#' discTransforms <- rxDiscretize(~ cost, data = claimsXdf, type = "width", nBins = 1000, subscript = "disc", sep = "_")
#' x <- rxDataStep(inData = claimsXdf, transforms = discTransforms)
#'
#' # Equal Freq
#' discTransforms <- rxDiscretize(~ cost, data = claimsXdf, type = "freq", nBins = 1000, subscript = "disc", sep = "_")
#' x <- rxDataStep(inData = claimsXdf, transforms = discTransforms)
rxDiscretize <- function (formula = ~ ., data, type = c("width", "freq"), 
  nBins = NULL, subscript = "disc", sep = "_", integerLabels = TRUE, finalTransform = TRUE) {
  rxList2Transform <- function(myList) {
    parse(text = paste("list(", paste(myList, collapse = ", "), ")"))
  }
  formula <- formulaExpand(formula, data)
  xVars <- all.vars(formula)
  y <- NULL
  if (length(formula) == 3) {
    y <- xVars[1]
    xVars <- xVars[-1]
  }
  varInfo <- rxGetInfo(data, getVarInfo = TRUE)
  n <- varInfo$numRows
  varInfo <- varInfo$varInfo
  xVarType <- unlist(lapply(varInfo[xVars], "[[", "varType"))
  if (!all(xVarType %in% c("integer", "numeric"))) {
    stop("Independent Variables must be Numeric")
  }
  type <- match.arg(type, c("width", "freq"))
  if (is.null(nBins)) {
    nBins <- floor(sqrt(n))
  }
  if (type == "width") {
    ## Equal Width
    varRange <- lapply(xVars, function(x) unlist(varInfo[[x]][c("low", "high")]))
    predBreaks <- lapply(varRange, function(x) seq(x["low"], x["high"], length = nBins + 1))
  } else {
    ## Equal Freq
    predBreaks <- lapply(xVars, function(x) unique(rxQuantile(data = data, varName = x,
	    probs = seq(from = 0, to = 1, length = nBins + 1), names = FALSE)))
  }
  
  names(predBreaks) <- xVars
  myFun <- function(x) {
    breaks <- paste(deparse(predBreaks[[x]]), collapse = "")
    labels <- paste(deparse(seq_len(length(predBreaks[[x]])-1)), collapse = "")
    paste(paste(x, subscript, sep = sep), " = cut(", x, 
      ", breaks = ", breaks, 
      ifelse(integerLabels, paste(", labels =", labels), ""),
      ", include.lowest = TRUE)", sep = "")
  }
  discTransforms <- lapply(xVars, myFun)
  if (finalTransform) {
    discTransforms <- rxList2Transform(discTransforms)
  }
  return(discTransforms)
}

