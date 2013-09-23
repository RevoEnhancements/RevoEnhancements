#
# RevoEnhancements/R/rxDTree.R by Andrie de Vries
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


#' Plots rxDTree object.
#' 
#' @param x rxDTree object
#' @param text If TRUE, adds text labels
#' @param plotArgs Named list, passed to \code{link[rpart]{plot.rpart}}
#' @param ... Other arguments, passed to \code{link[rpart]{text.rpart}}
#' @method plot rxDTree
#' @import rpart
#' @export
#' @examples
#' library("RevoScaleR")
#' frm <- as.formula(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width)
#' 
#' fit <- rxDTree(frm, data = iris)
#' plot(fit)
#' 
#' if(exists("rxDForest")){
#'   fit <- rxDForest(frm, data = iris)
#'   plot(fit$forest[[1]])
#'   plot(fit$forest[[3]])
#' }
plot.rxDTree <- function(x, text=TRUE, plotArgs, ...){
#   stopifnot(require(rpart))
  class(x) <- "rpart"
  x$functions$text <- function (yval, dev, wt, ylevel, digits, n, use.n) 
  {
#     browser()
    nclass <- 1
#     group <- yval[, 1L]
    group <- yval
    counts <- yval
    if (!is.null(ylevel)) 
      group <- ylevel[group]
    temp1 <- rpart:::formatg(counts, digits)
    if (nclass > 1L) 
      temp1 <- apply(matrix(temp1, ncol = nclass), 1L, paste, 
                     collapse = "/")
    if (use.n) 
      paste0(format(group, justify = "left"), "\n", temp1)
    else format(group, justify = "left")
  }
  if(!missing("plotArgs") && !(is.null(plotArgs))) plot(x, plotArgs) else plot(x)
  if(text) rpart:::text.rpart(x, ...)
}
