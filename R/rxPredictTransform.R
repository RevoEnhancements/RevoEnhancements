#
# RevoEnhancements/R/rxPredictTransform by Andrie de Vries
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

#' Model formula for rxDTree.
#' 
#' @param x \R object
#' @param ... further arguments passed to or from other methods
#' @method formula rxDTree
#' @export
formula.rxDTree <- function(x, ...){
  as.formula(x$params$Formula)
}


#' Checks if dependent variable is log transformed.
#' 
#' @param object An object, either a formula, or containing a formula that can be accessed with \code{formula(object)}
#' @export
isLogFormula <- function(object){
  UseMethod("isLogFormula", object)
}

#' @export
isLogFormula.default <- function(object){
  frm <- formula(object)
  stopifnot(length(frm) >= 3L)
  grepl("^log[\\(\\.].*[\\)\\.]", frm[2])
}


#' Applies a transformation on predicted values of model.
#' 
#' The function automatically detects whether the formula in a fitted model was log transformed, and applies the inverse, i.e. \code{exp(pred)}.
#' 
#' At the moment, always returns a vector, rather than a column in a data frame or XDF.
#' 
#' @param modelObject object returned from a call to \code{rxLinMod}, \code{rxLogit}, or \code{rxGlm}. Objects with multiple dependent variables are not supported in rxPredict.
#' @param data frame or XDF. New data to score.
#' @param logModel Logical. If TRUE, applies exponential transformation, i.e. \code{exp(pred)}
#' @param asVector Ignored
#' @param ... Passed to \code{rxPredict}
rxPredictTransform <- function(modelObject, data, logModel=isLogFormula(modelObject), asVector=TRUE, ...){
  pred <- rxPredict(modelObject, data, predVarNames = "pred", ...)$pred
  if(logModel) pred <- exp(pred)
  pred
}

