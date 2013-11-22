#' Extracts statistical moments and calculates skewness and kurtosis.
#' 
#' @param formula formula, as described in \code{\link[RevoScaleR]{rxFormula}}. The formula typically does not contain a response variable, i.e. it should be of the form ~ terms.
#' @param inData either a data source object, a character string specifying a '.xdf' file, or a data frame object to summarize.
#' @param ... Other arguments passed to \code{\link[RevoScaleR]{rxDataStep}}
#' @return list with names elements for
#' \describe{
#'   \item{moments}{list of 2nd, 3rd and 4th order moments}
#'   \item{skewness}{list with elements g1, G1 and b1.  These correspond to the definition used for skewness in 1) traditional textbooks, 2) SAS and SPSS and 3) Minitab and BDMP}
#'   \item{kurtosis}{list with elements g2, G2 and b2.  These correspond to the definition used for kurtosis in 1) traditional textbooks, 2) SAS and SPSS and 3) Minitab and BDMP}
#' }
#' @export
#' @family Data mining functions
#' @seealso \code{\link[RevoScaleR]{rxSummary}}
#' @examples
#' library(RevoScaleR)
#' if(require("e1071")){
#'   skewness(iris$Sepal.Length, type=3)
#'   skewness(iris$Sepal.Width, type=1)
#'   kurtosis(iris$Sepal.Length, type=3)
#'   kurtosis(iris$Sepal.Width, type=1)
#' }
#' rxMoments(~ Sepal.Length + Sepal.Width, iris)
#' rxMoments(~ Sepal.Width + Sepal.Length, iris)

rxMoments <- function(formula, inData=NULL, ...){
  
  .rxModify <- NULL # trick to pass R CMD check
  
  formVars <- all.vars(as.formula(formula))
  
  calcMoments <- function(datalist){
    n     <- vapply(datalist, function(x)length(x), FUN.VALUE=0, USE.NAMES=FALSE)
    sumx  <- vapply(datalist, function(x)sum(x),   FUN.VALUE=0, USE.NAMES=FALSE)
    sumx2 <- vapply(datalist, function(x)sum(x^2), FUN.VALUE=0, USE.NAMES=FALSE)
    sumx3 <- vapply(datalist, function(x)sum(x^3), FUN.VALUE=0, USE.NAMES=FALSE)
    sumx4 <- vapply(datalist, function(x)sum(x^4), FUN.VALUE=0, USE.NAMES=FALSE)
    .rxModify("n",     n,     FUN="+")
    .rxModify("sumx",  sumx,  FUN="+")
    .rxModify("sumx2", sumx2, FUN="+")
    .rxModify("sumx3", sumx3, FUN="+")
    .rxModify("sumx4", sumx4, FUN="+")
    
    return(NULL)
  }
  
  zz <- rxDataStep(inData, varsToKeep=formVars, 
                   transformFunc=calcMoments, 
                   transformObjects=list(
                     n = 0,
                     sumx  = rep(0, length(formVars)),
                     sumx2 = rep(0, length(formVars)),
                     sumx3 = rep(0, length(formVars)),
                     sumx4 = rep(0, length(formVars))
                   ),
                   returnTransformObjects=TRUE,
                   transformEnvir=new.env(),
                   ...
  )
  n     <- zz[["n"]]
  sumx  <- zz[["sumx"]]
  sumx2 <- zz[["sumx2"]]
  sumx3 <- zz[["sumx3"]]
  sumx4 <- zz[["sumx4"]]
  
  # m2 : sum(x^2) - 1/n * sum(x)^2
  # m3 : sum(x^3)  -  3/n * sum(x^2)*sum(x)  +  3/n^2 * sum(x)^3  -  1/n^2 * sum(x)^3
  # m4 : sum(x^4)  -  4/n * sum(x^3)*sum(x)  +  6/n^2 * sum(x^2)*sum(x)^2  -  4/n^3 * sum(x)^4  +  1/n^3 * sum(x)^4

  m2 <- (sumx2 - 1/n * sumx^2) / n
  m3 <- (sumx3 -3/n * sumx2*sumx + 3/n^2 * sumx^3 - 1/n^2 * sumx^3) / n
  m4 <- (sumx4 - 4/n * sumx3*sumx + 6/n^2 * sumx2*sumx^2 - 4/n^3 * sumx^4 + 1/n^3 * sumx^4) / n

  # skewness
  g1 <- m3 / m2^1.5                  # In textbooks
  G1 <- g1 * sqrt(n * (n-1)) / (n-2) # In SAS/SPSS
  b1 <- g1 * ((n-1)/n)^1.5           # In Minitab and BDMP
  
  # kurtosis
  g2 <- m4 / m2^2 - 3                        # In textbooks
  G2 <- ((n+1)*g2 + 6) * (n-1)/((n-2)*(n-3)) # In SAS/SPSS
  b2 <- (g2 + 3) * (1 - 1/n)^2 - 3           # In Minitab and BDMP
  
  
  list(
    moments  = list(m2=m2, m3=m3, m4=m4),
    skewness = list(g1=g1, G1=G1, b1=b1),
    kurtosis = list(g2=g2, G2=G2, b2=b2)
  )
  
}

