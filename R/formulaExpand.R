#
#  RevoEnhancements/R/compatibility.R by Derek Norton andAndrie de Vries  Copyright (C) 2012-2013
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
