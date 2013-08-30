#
# RevoEnhancements/R/formulaExpand.R by Derek Norton and Andrie de Vries
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


#' Expand a . to represent all variables in an XDF (DF).
#'
#' This function allows the use of . (dot) expansion in functions that would 
#' otherwise not allow it.
#'
#' @param formula formula to be expanded
#' @param data XDF file or data.frame from which to create new formula
#' @keywords expand, formula
#' @export
#' @family Compatibility functions
formulaExpand <- function(formula, data){
  expandPos <- length(formula)
  if (formula[[expandPos]] == ".") {
    formNames <- names(rxGetVarInfo(data))
    if (expandPos == 3) {
      formNames <- formNames[!formNames %in% as.character(formula[[2]])]
    }
    formNames <- paste(formNames, collapse = "+")
    formula <- as.formula(
      paste(ifelse(expandPos == 3, as.character(formula[[2]]), ""), "~", formNames),
      env = .GlobalEnv
    )
  }
  return(formula)
}
