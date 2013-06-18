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