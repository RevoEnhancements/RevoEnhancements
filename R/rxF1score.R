#
# RevoEnhancements/R/rxTreeDiscretize.R by Derek Norton and Andrie de Vries
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




#' Calculates machine learning statistics, including F1, accuracy and recall.
#'
#' @param actualVarName A character string with the name of the variable containing actual (observed) binary values.
#' @param predVarName A character string or vector of character strings with the name(s) of the variable containing predicted values in the [0,1] interval.
#' @param data data frame, character string containing an '.xdf' file name (with path), or RxXdfData object representing an '.xdf' file containing the actual and observed variables.
#' @param blocksPerRead number of blocks to read for each chunk of data read from the data source.
#' @param reportProgress Passed to \code{\link[RevoScaleR]{rxDataStep}}
#' @export
#' @return
#' A list with elements:
#' \describe{
#' \item{precision}{Precision}
#' \item{recall}{Recall}
#' \item{trueNegRate}{True negative rate}
#' \item{accuracy}{Accuracy}
#' \item{F1}{F1 score}
#' }
#' @family Model summary statistics
#' @examples
#' library(RevoScaleR)
#' 
#' ## Demonstrates calculation on data frame
#' 
#' dat <- data.frame(iris[, -5], Virg = iris$Species == "virginica")
#' fit <- rxLogit(Virg ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
#'                dat)
#' prd <- rxPredict(fit, dat)$Virg_Pred
#' cmb <- data.frame(Virg=dat$Virg, Pred=prd)
#' rxF1score("Virg", "Pred", cmb)
#' 
#' ## Demonstrates calculation on xdf file
#' 
#' dataFile <- tempfile(pattern = ".data", fileext = ".xdf")
#' rxDataStep(dat, outFile=dataFile, rowsPerRead=50)
#' rxGetInfo(dataFile)
#' fit <- rxLogit(Virg ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
#'                dataFile)
#' rxPredict(fit, data=dataFile, outData=dataFile)
#' rxGetInfo(dataFile)
#' rxGetVarInfo(dataFile)
#' rxF1score("Virg", "Virg_Pred", dataFile)
#' file.remove(dataFile)  

rxF1score <- function (actualVarName, predVarName, data, blocksPerRead = 1, 
                  reportProgress = rxGetOption("reportProgress")) 
{
  .rxGet <- function() {}
  .rxSet <- function() {}
  rm(.rxGet, .rxSet)
  f1BlockCompute <- function(datalist){
    x <- datalist[[actualVarName]]
    p <- datalist[[predVarName]]
    if(is.numeric(x)) x <- as.logical(round(x))
    if(is.numeric(p)) p <- as.logical(round(p))
    .rxSet("tp", .rxGet("tp") + sum(x==1 & p==1) )
    .rxSet("fp", .rxGet("fp") + sum(x==0 & p==1) )
    .rxSet("tn", .rxGet("tn") + sum(x==0 & p==0) )
    .rxSet("fn", .rxGet("fn") + sum(x==1 & p==0) )
    return(NULL)
  }
  ret <- rxDataStep(
    inData = data, 
    varsToKeep = c(actualVarName, predVarName), 
    blocksPerRead = blocksPerRead, 
    reportProgress = reportProgress, 
    returnTransformObjects = TRUE, 
    transformFunc = f1BlockCompute, 
    transformObjects = list(tp=0, fp=0, tn=0, fn=0)
  )
  tp <- ret[["tp"]]
  fp <- ret[["fp"]]
  tn <- ret[["tn"]]
  fn <- ret[["fn"]]
  precision   <- tp / (tp + fp)
  recall      <- tp / (tp + fn)
  trueNegRate <- tn / (tn + fp)
  accuracy    <- (tp + tn) / (tp + tn + fp + fn)
  
  F1 <- 2 * (precision * recall) / (precision + recall)
  
  list(
    precision   = precision,
    recall      = recall,
    trueNegRate = trueNegRate,
    accuracy    = accuracy,
    F1          = F1
  )
}