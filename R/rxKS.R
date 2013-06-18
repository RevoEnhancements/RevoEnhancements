#
#  RevoEnhancements/R/statistics.R by Derek Norton andAndrie de Vries  Copyright (C) 2012-2013
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


#' Generic method to calculate the KS Statistic.
#' 
#' @param x ???
#' @export
rxKS <- function (x) {
  UseMethod("rxKS")
}

#' Function to calculate the KS Statistic.
#' 
#' @param x ???
#' @export
#' @family Modelling functions
rxKS.rxLorenz <- function (x) {
  max(abs(x$percents - x$cumVals)) / 100
}