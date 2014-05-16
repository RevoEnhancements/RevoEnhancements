#' Calculates weight of evidence.
#' 
#' Calculates weight of evidence, similar to how SAS does it.
#' 
#' Information value is defined as IVx =∑_1:10 (bad_i − good_i) log (bad_i * good_i)
#' 
#' @param dependent logical or numeric vector in range [0, 1], i.e. a classifier.
#' @param group independent variable. Numeric vector of same length as \code{dependent}
#' @references http://opinions5.blogspot.co.uk/2011/09/information-value.html
#' @references http://support.sas.com/resources/papers/proceedings13/095-2013.pdf
woe <- function(dependent, group, iv=FALSE, breaks=10, probs=seq(0, 1, by=1/breaks), na.rm=TRUE){
  .woe <- function(x){
    nx     <- if(na.rm) length(!is.na(dependent)) else length(dependent)
    sumx   <- sum(1-x, na.rm=na.rm)
    sum1_x <- sum(x, na.rm=na.rm)
    log( (sumx/nx) / (sum1_x/nx) )
  }
  .iv <- function(x){
    nx     <- if(na.rm) length(!is.na(dependent)) else length(dependent)
    sumx   <- sum(1-x, na.rm=na.rm)
    sum1_x <- sum(x, na.rm=na.rm)
    ( (sumx/nx) - (sum1_x/nx) ) * log( (sumx/nx) / (sum1_x/nx) )
  }
  
  woeFunction <- if(iv) .iv else .woe
  
  if(!missing(group) && !is.null(group)){
    if(is.factor(group) | is.character(group)) {
      membership <- group
    } else {
      deciles <- quantile(group, probs=probs)
      membership <- findInterval(group, deciles, rightmost.closed=TRUE)
    }
    ret <- tapply(dependent, membership, FUN=woeFunction)
    sum(ret)
  } else {
    woeFunction(dependent)
  }
}

#' Calculates information value.
#' 
#' This is a wrapper around \code{\link{woe}}.
iv <- function(dependent, group, iv=TRUE, breaks=10, probs=seq(0, 1, by=1/breaks), na.rm=TRUE){
  if(missing(group)){
    woe(dependent=dependent, iv=TRUE, probs=probs, na.rm=na.rm)
  } else {
    woe(dependent=dependent, group=group, iv=TRUE, probs=probs, na.rm=na.rm)
  }
}


# Test results ------------------------------------------------------------

with(iris, woe(Species=="setosa"))
with(iris, woe(Species=="setosa", group=Sepal.Width))

with(iris, iv(Species=="setosa"))
with(iris, iv(Species=="setosa", group=Sepal.Width))
     



