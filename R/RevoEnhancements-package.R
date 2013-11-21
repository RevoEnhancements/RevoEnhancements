#
# RevoEnhancements/R/RevoEnhancements-package.R by Derek Norton and Andrie de Vries
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


#' This package adds functionality to RevoScaleR. All functions require, and are enhancements to, [Revolution R Enterprise](http://www.revolutionanalytics.com/products/revolution-enterprise.php).
#' 
#' This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 or 3 of the License (at your option).
#'
#'  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
#'
#'  A copy of the GNU General Public License is available at http://www.r-project.org/Licenses/

#'
#' Areas of enhancement are:
#' 
#' \itemize{
#' \item{CRAN R Compatability}
#' \item{Big Data Graphics}
#' \item{Big Data Mining}
#' \item{Discretization}
#' \item{Variable Importance / Selection}
#' \item{Models}
#' \item{Scoring}
#' \item{...}
#' }
#'
#' Some functions you might find useful:
#' 
#' Compatibility and utility functions:
#' \itemize{
#' \item{\code{\link{formulaExpand}}}
#' \item{\code{\link{range2formula}}}
#' }
#' 
#' Modeling:
#' \itemize{
#' \item{\code{\link{rxKS}}}
#' \item{\code{\link{rxTTest}}}
#' \item{\code{\link{rxMoments}}}
#' }
#' 
#' Data visualisation:
#' \itemize{
#' \item{\code{\link{rxBoxPlot}}}
#' \item{\code{\link{rxHexBin}}}
#' }
#' 
#' Data mining:
#' \itemize{
#' \item{\code{\link{rxUnique}}} to retrieve unique values in columns
#' \item{\code{\link{rxDiscretize}}} to discretize (bin) values 
#' \item{\code{\link{rxSample2Df}}} to sample values to a data frame
#' \item{\code{\link{rxTreeDiscretize}}} to discretize a tree object
#' \item{\code{\link{rxF1score}}} to calculate model precision, sensitivity and F1 score
#' \item{\code{\link{rxLinPredError}}} to calculate prediction error for linear models
#' }


#' 
#' 
#' @name RevoEnhancements-package
#' @aliases RevoEnhancements
#' @docType package
#' @title Adds functionality to RevoScaleR
#' @author Derek Norton \email{derek.norton@@revolutionanalytics.com} and Andrie de Vries \email{andrie@@revolutionanalytics.com}
#' @keywords package
#' @import RevoScaleR
NULL
