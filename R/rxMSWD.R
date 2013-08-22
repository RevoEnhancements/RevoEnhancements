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


#' \code{rxMSWD} Calculates mean square weighted deviation from predicted, response and weights.
#'
#' @param actualVarName String name of the response variable.
#' @param predVarName String name of the predicted variable.
#' @param sWeights String name of error weights
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
#' dat <- data.frame(Sepal.Length=iris$Sepal.Length, Sepal.Length_Pred=prd, Weights = rep(1, nrow(iris))/nrow(iris))
#' rxMSWD("Sepal.Length", "Sepal.Length_Pred", "Weights", dat)
#'

rxMSWD <- function (actualVarName, predVarName, sWeights, data, blocksPerRead = 1,
                   reportProgress = rxGetOption("reportProgress")) 
{
  
  .BlockCompute <- function(datalist){
    
    # Getting the data
    dActualY <- datalist[[actualVarName]]
    dPredY <- datalist[[predVarName]]
    dWeights <- datalist[[sWeights]]
    
    # Error
    dError <- (dPredY - dActualY)
    # Missing boolean
    bMissing <- is.na(dError)
    
    # Keeping only non-missing data
    dError <- dError[!bMissing]
    dWeights <- dWeights[!bMissing]
    
    # Weighted errors
    dSumSQWeightedErrors <- sum(dWeights*(dError^2))
    
    .rxSet("dSumWeights", .rxGet("dSumWeights") + sum(dWeights))
    .rxSet("dSumSQWeightedErrors", .rxGet("dSumSQWeightedErrors") + dSumSQWeightedErrors)
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
    transformObjects = list(dSumWeights = 0, dSumSQWeightedErrors = 0, 
                            N = 0)
  )
  
  # Retreiving the values
  dSumWeights <- ret[["dSumWeights"]]
  dSumSQWeightedErrors <- ret[["dSumSQWeightedErrors"]]
  N <- ret[["N"]]
  
  # Need MSE for sample variance
  MSE <- rxMSE(actualVarName = actualVarName, predVarName = predVarName, data = data)
  MSWD <- (dSumSQWeightedErrors)*((N-1)*dSumWeights/N)
  
  list(MSWD = MSWD, SumWeights = dSumWeights, 
       SumSQWeightedErrors = dSumSQWeightedErrors,
       N = MSE$N)
}
