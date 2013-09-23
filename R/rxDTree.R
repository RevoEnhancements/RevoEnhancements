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
#' @param plotArgs Named list, passed to \code{\link[rpart]{plot.rpart}}
#' @param textArgs Names list, passed to \code{\link[rpart]{text.rpart}}
#' @param ... Not used. Required for consistency with other plot methods.
#' @method plot rxDTree
#' @import rpart
#' @export
#' @examples
#' 
#' ### rxDTree examples
#' library("RevoScaleR")
#' library("ggplot2")
#' frm <- formulaExpand(price ~ ., data=diamonds)
#' fit <- rxDTree(frm, diamonds, maxDepth=3)
#' par(mar=c(0.5, 0.5, 2, 0.5))
#' plot(fit)
#' plot(fit, textArgs=list(col="blue", cex=0.7))
#' plot(fit, textArgs=list(col="blue", cex=0.7), plotArgs=list(main="Forest 1"))
#' 
#' 
#' if(exists("rxDForest")){
#'   fit <- rxDForest(frm, diamonds, maxDepth=3)
#'   forest <- fit$forest
#'   par(mar=c(0.5, 0.5, 2, 0.5))
#'   plot(forest[[1]])
#'   plot(forest[[1]], textArgs=list(col="blue", cex=0.7))
#'   plot(forest[[1]], textArgs=list(col="blue", cex=0.7), plotArgs=list(main="Forest 1"))
#'   plot(forest[[2]], textArgs=list(col="blue", cex=0.7), plotArgs=list(main="Forest 2"))
#' }
plot.rxDTree <- function(x, text=TRUE, plotArgs=NULL, textArgs=NULL, ...){
#   stopifnot(require(rpart))
  class(x) <- "rpart"
  x$functions$text <- function (yval, dev, wt, ylevel, digits, n, use.n) 
  {
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
  dots <- match.call()
  ### Create plot
  if(!missing(plotArgs)) {
    do.call(plot, c(list(x=x), plotArgs)) 
  } else {
    plot(x)
  }
  ### Add text
  if(text) {
    if(!missing(textArgs)) {
      do.call(rpart:::text.rpart, c(list(x=x), textArgs)) 
    } else {
      text(x)
    }
  }
}
