#
# RevoEnhancements/R/range2formula.R by Derek Norton and Andrie de Vries
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


#' Create a formula from a range on an XDF (DF).
#'
#' This function allows the specification of a range of variable instead of a formula
#'
#' @param range numeric range of values to use in formula
#' @param data XDF file or data.frame from which to create new formula
#' @keywords range, formula
#' @export
#' @family Compatibility functions
range2formula <- function(range, data){
  formNames <- names(rxGetVarInfo(data))[range]
  formNames <- paste(formNames, collapse = "+")
  return(as.formula(paste("~", formNames))) 
}