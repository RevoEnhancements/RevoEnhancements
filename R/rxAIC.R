#
# RevoEnhancements/R/rxAIC by Derek Norton
#
# Copyright 2013-2014 Revolution Analytics
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


#' Calculates Akaike's Information Criterium (AIC) from a model object.
#' 
#' @param object Model object, the result of \code{\link[RevoScaleR]{rxLogit}}, \code{\link[RevoScaleR]{rxLinMod}} or \code{\link[RevoScaleR]{rxGlm}}
#' @param k numeric, the penalty per parameter to be used; the default k = 2 is the classical AIC
#' @return Numeric
#' @export
#' @family Model summary statistics
#' @seealso \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}}
#' @examples
#' library(RevoScaleR)
#' sampleDataDir <- rxGetOption("sampleDataDir")
#' airline <- file.path(sampleDataDir, "AirlineDemoSmall.xdf")
#' frm <- formulaExpand(ArrDelay ~ ., airline)
#' model <- rxLinMod(frm, airline)
#' rxAIC(model)
rxAIC <- function(object, k = 2) {
  deviance(object) + k * object$df[1]
}
