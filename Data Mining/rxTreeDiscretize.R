# Function to discretize continous variables using a tree.

rxTreeDiscretize <- function(formula, data, criteria = c("gini", "information"),
  subscript = "disc", sep = "_", integerLabels = FALSE, finalTransform = TRUE){
  if (length(formula) != 3) {
    stop("Dependent and Independent Variables Are Required in the Formula")
  }
  rxList2Transform <- function(myList) {
    parse(text = paste("list(", paste(myList, collapse = ", "), ")"))
  }
  varInfo <- rxGetVarInfo(data)
  xVars <- all.vars(formula)
  y <- xVars[1]
  yType <- varInfo[[y]][["varType"]]
  xVars <- xVars[-1]
  xVarType <- unlist(lapply(varInfo[xVars], "[[", "varType"))
  if (!all(xVarType %in% c("integer", "numeric"))) {
    stop("Independent Variables must be Numeric")
  }
  if (yType %in% c("integer", "numeric")) {
    stop("The Dependent Variable must be Categorical")
  }
  criteria <- match.arg(criteria, c("gini", "information"))
  # add an lapply for multiple x's
  treeFun <- function(x) {
    treeMod <- rxDTree(as.formula(paste(y, "~", x)), data = data,
      parms = list(split = criteria), reportProgress = 0)$splits[,"index"]
    predBreaks <- c(-Inf, as.numeric(sort(treeMod)), Inf)
    return(predBreaks)
  }
  predBreaks <- lapply(xVars, treeFun)
  names(predBreaks) <- xVars
  myFun <- function(x) {
    breaks <- paste(deparse(predBreaks[[x]]), collapse = "")
    labels <- paste(deparse(seq_len(length(predBreaks[[x]])-1)), collapse = "")
    paste(paste(x, subscript, sep = sep), " = cut(", x, 
      ", breaks = ", breaks, 
      ifelse(integerLabels, paste(", labels =", labels), ""),
      ")", sep = "")
  }
  discTransforms <- lapply(xVars, myFun)
  if (finalTransform) {
    discTransforms <- rxList2Transform(discTransforms)
  }
  return(discTransforms)
}

## Examples
#claimsXdf <- file.path(rxGetOption("sampleDataDir"),"claims.xdf")
#claimsXdf <- RxXdfData(claimsXdf)
#discTransforms <- rxTreeDiscretize(type ~ cost + number, data = claimsXdf)
#discTransforms2 <- rxTreeDiscretize(type ~ cost + number, data = claimsXdf, criteria = "information")
#newData <- rxDataStep(inData = claimsXdf, transforms = discTransforms)
