#
# RevoEnhancements/R/rxMAPE by Chibisi Chima-Okereke
# 
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


#' \code{rxMAPE} Calculates mean absolute percentage error from predicted and response variable.
#'
#' @param actualVarName String name of the response variable.
#' @param predVarName String name of the predicted variable
#' @param data data frame, or character string containing an '.xdf' file name (with path), or RxXdfData object representing an '.xdf' file containing the actual and observed variables.
#' @param blocksPerRead number of blocks to read for each chunk of data read from the data source.
#' @param reportProgress Passed to \code{\link{rxDataStep}}
#' @export
#' @family Data mining functions
#' @examples
#' library(RevoScaleR)
#' 
#' ## Demonstrates calculation on data frame
#' 
#' fit <- rxLinMod(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
#' prd <- rxPredict(fit, iris)$Sepal.Length_Pred
#' dat <- data.frame(Sepal.Length=iris$Sepal.Length, Sepal.Length_Pred=prd)
#' rxMAPE("Sepal.Length", "Sepal.Length_Pred", dat)
#' 
#' ## Demonstrates calculation on xdf file
#' 
#' dataFile <- tempfile(pattern = ".data", fileext = ".xdf")
#' rxDataStep(iris, outFile=dataFile, rowsPerRead=50)
#' rxGetInfo(dataFile)
#' fit <- rxLinMod(Sepal.Length ~ Petal.Length + Petal.Width, data = dataFile)
#' rxPredict(fit, data=dataFile, outData=dataFile)
#' rxGetInfo(dataFile)
#' rxGetVarInfo(dataFile)
#' rxMAPE("Sepal.Length", "Sepal.Length_Pred", dataFile)
#' file.remove(dataFile) 

rxMAPE <- function (actualVarName, predVarName, data, blocksPerRead = 1,
                   reportProgress = rxGetOption("reportProgress")) 
{
  
  .BlockCompute <- function(datalist){
    
    # Getting the data
    dActualY <- datalist[[actualVarName]]
    dPredY <- datalist[[predVarName]]
    
    # Error
    dError <- (dPredY - dActualY)
    
    # Missing bolean
    bMissing <- is.na(dError)
    
    # Keeping only non-missing data
    dActualY <- dActualY[!bMissing]
    dError <- dError[!bMissing]
    
    dSumPropError <- sum(abs(dError/dActualY))
    
    .rxSet("dSumPropError", .rxGet("dSumPropError") + dSumPropError)
    .rxSet("N", .rxGet("N") + length(dError))
    
    return(NULL)
  }
  
  ret <- rxDataStep(
    inData = data, 
    varsToKeep = c(actualVarName, predVarName), 
    blocksPerRead = blocksPerRead, 
    reportProgress = reportProgress, 
    returnTransformObjects = TRUE, 
    transformFunc = .BlockCompute, 
    transformObjects = list(dSumPropError = 0, N = 0)
  )
  
  dSumPropError <- ret[["dSumPropError"]]
  N <- ret[["N"]]
  MAPE <- 100*dSumPropError/N
  
  list(MAPE = MAPE, SumPropError = dSumPropError, N = N)
}
