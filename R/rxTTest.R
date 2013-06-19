#
# RevoEnhancements/R/rxTTest.R by Derek Norton and Andrie de Vries
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


#' Function to carry out a student's t test on an xdf file
#' 
#' @param formula ???
#' @param data ???
#' @param alternative ???
#' @param mu ???
#' @param var.equal ???
#' @param conf.level ???
#' @param ... ???
#' @export
#' @family Modelling functions
#' @examples
#' # Generate Data and test differences
#' library(RevoScaleR)
#' xdfFile <- "myTTestFile.xdf"
#' x <- data.frame(x = rnorm(1000, mean = -1, sd = 1), y = rnorm(1000, mean = 6, sd = 3), group = factor(rbinom(1000, 1, .6)))
#' rxDataStep(inData = x, outFile = xdfFile, overwrite = TRUE)
#'
#' # One-Sample t-test
#' t.test(x$x)
#' rxTTest(~ x, data = xdfFile)
#'
#' # Two-Sample t-test
#' t.test(x$x, x$y)
#' rxTTest(~ x + y, data = xdfFile)
#'
#' # Grouped Two-Sample t-test
#' t.test(x ~ group, data = x)
#' rxTTest(x ~ group, data = xdfFile)
#'
#' # Paired t-test
#' t.test(x$x, x$y, paired = TRUE)
#' rxTTest(~ I(x - y), data = xdfFile)
#'
#' # Grouped Two-Sample Paired t-test
#' # Not available in t.test()
#' rxTTest(I(x - y) ~ group, data = xdfFile)
#'
# unlink(xdfFile)
rxTTest <- function(formula, data, alternative = c("two.sided", "less", "greater"),
       mu = 0, var.equal = FALSE, conf.level = 0.95, ...){
  alternative <- match.arg(alternative)
  dname <- NULL
  if (!missing(mu) && (length(mu) != 1 || is.na(mu))) 
      stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || 
      conf.level < 0 || conf.level > 1)) 
      stop("'conf.level' must be a single number between 0 and 1")
  cl <- match.call()
  m <- match(c("alternative", "mu", "paired", "var.equal", "conf.level"), names(cl), 0L)
  if(any(m > 0)) {
	  cl <- cl[-m]
  }
  cl[[1L]] <- as.name("rxSummary")
  stats <- eval(cl, parent.frame())
  grouped <- length(stats$categorical)
  twoSample <- as.logical(length(grep("+", formula, fixed = TRUE)))
  if (twoSample){
    oneSample <- FALSE
    paired <- FALSE
  } else {
    oneSample <- !grouped
    pform <- grep("-", formula, fixed = TRUE)
    paired <- as.logical(length(pform))
    if (paired) {
      pform <- formula[[pform]]
      dname <- paste(all.vars(pform), collapse = " and ")
    }
  }
  if(oneSample){
    stats <- stats$sDataFrame
    if (is.null(dname)) dname <- all.vars(formula)
    mx <- stats$Mean
    nx <- stats$ValidObs
    vx <- stats$StdDev^2
    df <- nx - 1
    estimate <- mx
    stderr <- sqrt(vx/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx)) 
              stop("data are essentially constant")
    tstat <- (mx - mu)/stderr
    method <- ifelse(paired, "Paired t-test", "One Sample t-test")
    names(estimate) <- ifelse(paired, "mean of the differences", 
              "mean of x")
  } else {
    if (grouped) {
      stats <- stats$categorical[[1]]
      names(stats)[3L] <- "Mean"
      dname <- paste(paste(formula[[2L]]), names(stats)[2L], sep = " by ")
    } else {
      stats <- stats$sDataFrame
      dname <- paste(stats[[1L]], collapse = " and ")
    }
    mx <- stats$Mean[1]
    my <- stats$Mean[2]
    nx <- stats$ValidObs[1]
    ny <- stats$ValidObs[2]
    vx <- stats$StdDev[1]^2
    vy <- stats$StdDev[2]^2
    if (nx < 1 || (!var.equal && nx < 2)) 
        stop("not enough 'x' observations")
    if (ny < 1 || (!var.equal && ny < 2)) 
        stop("not enough 'y' observations")
    if (var.equal && nx + ny < 3) 
        stop("not enough observations")
    method <- paste(if (!var.equal) 
        "Welch", if (paired) "Paired", "Two Sample t-test")
    estimate <- c(mx, my)
    names(estimate) <- if (grouped){
        paste(ifelse(paired, 
          "mean of the differences in group", 
          "mean in group"), stats[[2L]])
      } else {
        paste("mean of", stats[[1L]])
      }
    if (var.equal) {
      df <- nx + ny - 2
      v <- 0
      if (nx > 1) 
          v <- v + (nx - 1) * vx
      if (ny > 1) 
          v <- v + (ny - 1) * vy
      v <- v/df
      stderr <- sqrt(v * (1/nx + 1/ny))
    }
    else {
      stderrx <- sqrt(vx/nx)
      stderry <- sqrt(vy/ny)
      stderr <- sqrt(stderrx^2 + stderry^2)
      df <- stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 1))
    }
    if (stderr < 10 * .Machine$double.eps * max(abs(mx), 
      abs(my))) 
      stop("data are essentially constant")
    tstat <- (mx - my - mu)/stderr
  }
  if (alternative == "less") {
      pval <- pt(tstat, df)
      cint <- c(-Inf, tstat + qt(conf.level, df))
  }
  else if (alternative == "greater") {
      pval <- pt(tstat, df, lower.tail = FALSE)
      cint <- c(tstat - qt(conf.level, df), Inf)
  }
  else {
      pval <- 2 * pt(-abs(tstat), df)
      alpha <- 1 - conf.level
      cint <- qt(1 - alpha/2, df)
      cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- if (paired || !oneSample) 
      "difference in means"
  else "mean"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = df, p.value = pval, 
      conf.int = cint, estimate = estimate, null.value = mu, 
      alternative = alternative, method = method, data.name = paste(dname, data, sep = " from "))
  class(rval) <- "htest"
  return(rval)
}

