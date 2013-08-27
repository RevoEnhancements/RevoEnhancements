#
# RevoEnhancements/R/rxMSWD by Chibisi Chima-Okereke
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


#' rxLinPredError Calculates MSE, MAPE, MPE, and MSWD
#'
#' @param actualVarName String name of the response variable.
#' @param predVarName String name of the predicted variable.
#' @param sWeights String name of error weights
#' @param data data frame, or character string containing an '.xdf' file name (with path), or RxXdfData object representing an '.xdf' file containing the actual and observed variables.
#' @param blocksPerRead number of blocks to read for each chunk of data read from the data source.
#' @param reportProgress Passed to \code{\link{rxDataStep}}
#' @return returns a list of prediction measures MSE, MAPE, MPE, MSWD 
#' @export
#' @family Data mining functions
#' @examples
#' library(RevoScaleR)
#' 
#' ## Demonstrates calculation on data frame
#' 
#' fit <- rxLinMod(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
#' prd <- rxPredict(fit, iris)$Sepal.Length_Pred
#' dat <- data.frame(Sepal.Length=iris$Sepal.Length, Sepal.Length_Pred=prd, Weights = rep(1, nrow(iris))/nrow(iris))
#' rxMSWD("Sepal.Length", "Sepal.Length_Pred", "Weights", dat)
#'

rxLinPredError <- function (actualVarName, predVarName, sWeights = NULL, data, blocksPerRead = 1,
                    reportProgress = rxGetOption("reportProgress")) 
{
  
  .BlockCompute <- function(datalist){
    
    # Getting the data
    dActualY <- datalist[[actualVarName]]
    dPredY <- datalist[[predVarName]]
        
    # Error
    dError <- (dPredY - dActualY)
    # Missing boolean
    bMissing <- is.na(dError)
    
    # Keeping only non-missing data
    dError <- dError[!bMissing]
    
    # Weights
    if(is.null(sWeights)){
      dWeights <- rep(1, length(dError))/length(dError)
    }else{
      dWeights <- datalist[[sWeights]]
    }
    
    dWeights <- dWeights[!bMissing]
    dActualY <- dActualY[!bMissing]
    
    # For MAPE
    dSumABSPropError <- sum(abs(dError/dActualY))
    # For MPE
    dSumPropError <- sum(dError/dActualY)
    # For RSS
    RSS <- sum(dError^2)
    
    # Weighted errors
    dSumSQWeightedErrors <- sum(dWeights*(dError^2))
    
    .rxSet("dSumABSPropError", .rxGet("dSumABSPropError") + dSumABSPropError)
    .rxSet("dSumPropError", .rxGet("dSumPropError") + dSumPropError)
    .rxSet("dSumWeights", .rxGet("dSumWeights") + sum(dWeights))
    .rxSet("dSumSQWeightedErrors", .rxGet("dSumSQWeightedErrors") + dSumSQWeightedErrors)
    .rxSet("RSS", .rxGet("RSS") + RSS)
    .rxSet("N", .rxGet("N") + length(dError))
    
    return(NULL)
  }
  
  ret <- rxDataStep(
    inData = data, 
    varsToKeep = c(actualVarName, predVarName, sWeights), 
    blocksPerRead = blocksPerRead, 
    reportProgress = reportProgress, 
    returnTransformObjects = TRUE, 
    transformFunc = .BlockCompute, 
    transformObjects = list(dSumABSPropError = 0, dSumPropError = 0, dSumWeights = 0,
                            dSumSQWeightedErrors = 0, RSS = 0, N = 0)
  )
  
  # Retreiving the values
  dSumABSPropError <- ret[["dSumABSPropError"]]
  dSumPropError <- ret[["dSumPropError"]]
  dSumWeights <- ret[["dSumWeights"]]
  dSumSQWeightedErrors <- ret[["dSumSQWeightedErrors"]]
  RSS <- ret[["RSS"]]
  N <- ret[["N"]]
  
  
  MAPE <- dSumABSPropError/N
  MPE <- dSumPropError/N
  MSE <- RSS/N
  MSWD <- (dSumSQWeightedErrors)*((N-1)*dSumWeights/N)
  
  list(MAPE = MAPE, MPE = MPE, MSE = MSE, MSWD = MSWD)
}

rxLinPredError("Sepal.Length", "Sepal.Length_Pred", data = dat)
