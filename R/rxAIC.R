#
# RevoEnhancements/R/rXAIC by Derek Norton
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


#' Calculates Akaikes Information Criterium (AIC) from a model object.
#' 
#' @param object Model object, the result of \code{\link[RevoScaleR]{rxLogit}}, \code{\link[RevoScaleR]{rxLinMod}} or \code{\link[RevoScaleR]{rxGlm}}
#' @param k Multiplier. Defaults to 2, the value for AIC
#' @return Numeric
#' @export
#' @family Model summary statistics
#' @examples
#' library(RevoScaleR)
#' sampleDataDir <- rxGetOption("sampleDataDir")
#' working.file <- file.path(sampleDataDir, "AirlineDemoSmall.xdf")
rxAIC <- function(object, k = 2) {
  deviance(object) + k * object$df[1]
}
