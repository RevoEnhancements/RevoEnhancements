#' Naive Bayes.
#' 
#' Implementation of naive bayes using package e1071.
#' 
#' Naive Bayes is a simple probabilistic classifier based on applying Bayes' theorem with strong (naive) independence assumptions. This is often a good benchmark for other more complicated data mining models.
#'
#' Really you are just calculating proportions for categorical variables (with possible Laplace correction), and probabilities based on a normal distribution for numeric variables. The proportions are easily calculated using rxCrossTabs, and the normal probabilities are easily calculated given a mean and standard deviation which we can get from rxSummary.
#'
#' We can use existing \code{e1071} code and replace the calculation of proportions and probabilities with big data versions. The results are not only not big data, but existing methods work on object!
#' 
#' @param formula formula
#' @param data data frame or XDF
#' @param laplace positive double controlling Laplace smoothing. The default (0) disables Laplace smoothing
#' @param ... not used
#' @export
#' @family machine-learning
#' @examples
#' data(HouseVotes84, package = "mlbench")
#' hv <- HouseVotes84
#' hv$V17 <- rnorm(nrow(hv), mean = c(-3, 5)[as.numeric(hv$Class)], sd = c(.5, 2)[as.numeric(hv$Class)])
#' hv$V18 <- rnorm(nrow(hv), mean = c(2, 15)[as.numeric(hv$Class)], sd = c(4, 1)[as.numeric(hv$Class)])
#' model1 <- naiveBayes(Class ~ ., data = hv)
#' model2 <- rxNaiveBayes(Class ~ ., data = hv)
#' summary(model1)
#' summary(model2)
rxNaiveBayes <- function (formula, data, laplace = 0, ...) {
  require(e1071)
  call <- match.call()
  vars <- all.vars(formula)
  Yname <- vars[1]
  x <- vars[-1]
  varInfo <- rxGetVarInfo(data)
  if (x == ".") {
    x <- names(varInfo)
    x <- x[!x %in% Yname]
  }
  origOrder <- x
  catVars <- (sapply(varInfo, "[[", "varType") == c("factor"))[x]
  catVars <- catVars[order(catVars, decreasing = TRUE)]
  x <- names(catVars)
  catLength <- sapply(varInfo[names(which(catVars))], function(x) length(x$levels))
  sumVars <- list(categorical = x[catVars], numeric = x[!catVars])
  est <- function(vars) {
    catSum <- numSum <- NULL
    if (!is.null(vars[["categorical"]])) {
      catFun <- function(x) {
        form <- as.formula(paste("~", paste(Yname, x, sep = ":")))
        tab <- rxCrossTabs(form, data, returnXtabs = TRUE)
        class(tab) <- "table"
        attr(tab, "call") <- NULL
        (tab + laplace)/(rowSums(tab) + laplace * catLength[x])
      }
      catSum <- lapply(vars[["categorical"]], catFun)
    }
    if (!is.null(vars[["numeric"]])) {
      form <- as.formula(paste("~", paste(vars[["numeric"]], Yname, sep = ":", collapse = "+")))
      numVars <- rxSummary(form, data)$categorical
      numFun <- function(x) {
        ret <- as.matrix(x[, c("Means", "StdDev")])
        myNames <- vector("list", 2)
        myNames[[1]] <- x[, 2]
        dimnames(ret) <- myNames
        return(ret)
      }
      numSum <- lapply(numVars, numFun)
    } 
    ret <- c(catSum, numSum)
  }
  form <- as.formula(paste("~", Yname))
  apriori <- rxCrossTabs(form, data, returnXtabs = TRUE)
  class(apriori) <- "table"
  attr(apriori, "call") <- NULL
  tables <- est(sumVars)
  names(tables) <- x
  for (i in 1:length(tables)) names(dimnames(tables[[i]])) <- c("Y", x[i])
  names(dimnames(apriori)) <- "Y"
  structure(list(apriori = apriori, tables = tables, levels = varInfo[[Yname]][["levels"]], 
                 call = call), class = c("rxNaiveBayes", "naiveBayes"))
}
