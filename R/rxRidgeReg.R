#' Fits ridge regression model.
#' 
#' Fits ridge regression model using the correlation matrix.  Supports estimation of multiple values of the regularisation coefficient (lambda).
#' 
#' @param formula Model formula
#' @param data Data frame or XDF
#' @param lambda Regularisation coefficient.  Can be a single value or a vector.
#' @param ... Passed to \code{\link[RevoScaleR]{rxCovCor}}
#' @references http://blog.revolutionanalytics.com/2014/03/extending-revoscaler-ridge-regression.html
#' @author Derek McRae Norton
#' @export
#' 
rxRidgeReg <- function(formula, data, lambda, ...) {
  myTerms <- all.vars(formula)
  newForm <- as.formula(paste("~", paste(myTerms, collapse = "+")))
  myCor <- rxCovCor(newForm, data = data, type = "Cor", ...)
  n <- myCor$valid.obs
  k <- nrow(myCor$CovCor) - 1
  bridgeprime <- do.call(rbind, lapply(lambda, 
                                       function(l) qr.solve(myCor$CovCor[-1,-1] + l*diag(k), 
                                                            myCor$CovCor[-1,1])))
  bridge <-  myCor$StdDevs[1] * sweep(bridgeprime, 2, 
                                      myCor$StdDevs[-1], "/")
  bridge <- cbind(t(myCor$Means[1] - 
                      tcrossprod(myCor$Means[-1], bridge)), bridge)
  rownames(bridge) <- format(lambda)
  obj <- bridge
  class(obj) <- c("rxRidgeReg", "matrix")
  obj
}


plot.rxRidgeReg <- function(dat, scale=FALSE){
  if(colnames(dat)[1] == "") colnames(dat)[1] <- "Intercept"
  if(scale) {
    scaleValue <- apply(dat, 2, function(x){
      max(max(x), abs(min(x)))
    })
    dat <- scale(dat, center=FALSE, scale=scaleValue)
    ylab <- "Scaled coefficient"
  } else {
    ylab <- "Coefficient"
  }
  
  matplot(x=as.numeric(rownames(dat)), y=dat, type="l", bty="n",
          ylab=ylab, xlab="lambda")
  
}

#' @examples
#' data(swiss)
#' frm <- formulaExpand(Fertility ~ ., data=swiss)
#' rxRidgeReg(frm, swiss, lambda=0)
#' model <- rxRidgeReg(frm, swiss, lambda=c(seq(from=0, to=1, by=0.05)))
#' plot(model)
#' plot(model, scale=TRUE)


