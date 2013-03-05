# Function to create a Naive Bayes model on big data.  It uses e1071.

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

#data(HouseVotes84, package = "mlbench")
#x <- HouseVotes84
#x$V17 <- rnorm(nrow(x), mean = c(-3, 5)[as.numeric(x$Class)], sd = c(.5, 2)[as.numeric(x$Class)])
#x$V18 <- rnorm(nrow(x), mean = c(2, 15)[as.numeric(x$Class)], sd = c(4, 1)[as.numeric(x$Class)])

#model <- naiveBayes(Class ~ ., data = x)
#model2 <- rxNaiveBayes(Class ~ ., data = x)