#' Extracts unique values from character and factor columns in xdf.
#' 
#' @param formula formula, as described in \code{\link[RevoScaleR]{rxFormula}}. The formula typically does not contain a response variable, i.e. it should be of the form ~ terms.
#' @param inData either a data source object, a character string specifying a '.xdf' file, or a data frame object to summarize.
#' @param ... Other arguments passed to \code{\link[RevoScaleR]{rxDataStep}}
#' @return list with element for each named row in the formula
#' @export
#' @family Data mining functions
#' @examples
#' library(RevoScaleR)
#' dataSource <- file.path(rxGetOption("sampleDataDir"), "CensusWorkers.xdf")
#' rxGetVarInfo(dataSource)
#' rxUnique(~sex, inData=dataSource)
#' rxUnique(~sex+state, inData=dataSource)
#' rxUnique(~state+sex, inData=dataSource)
rxUnique <- function(formula, inData=NULL, ...){
  
  .rxModify <- NULL # trick to pass R CMD check

  formVars <- all.vars(as.formula(formula))
  .unique <- function(datalist){
    .cUnique <- function(x, y){
      x <- unique(as.character(x))
      if(!missing("y") && !is.null(y)){
        y <- unique(as.character(y))
        unique(c(x, y))
      } else {
        unique(x)
      }
    }
    .combine <- function(x, y){
      lapply(seq_along(x), function(i) .cUnique(x[[i]], y[[i]]))
    }
    
    ret <- lapply(datalist, function(x)unique(as.character(x)))
    
   .rxModify("vars", ret, FUN=.combine)
    return(NULL)
  }
  zz <- rxDataStep(inData, varsToKeep=formVars, 
                   transformFunc=.unique, 
                   transformObjects=list(
                     vars=setNames(
                       lapply(seq_along(formVars), function(i)character(0)), 
                       formVars)
                   ),
                   returnTransformObjects=TRUE,
                   transformEnvir=new.env(),
                   ...
  )
  setNames(zz$vars, formVars)
}



