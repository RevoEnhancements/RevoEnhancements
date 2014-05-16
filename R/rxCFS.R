entropy <- function (p) {
  safelog2 <- function (x) {
    if (x <= 0) return(0)
    else return(log2(x))
  }
  return(-(sum(p * sapply(p, safelog2))))
}


merit <- function(y, x) {
  k <- length(x)
  rcf
  sqrt(k + k(k - 1)*rff)
}

#' Correlation feature selection.
#' 
#' @param formula Model formula
#' @param data XDF
#' @param searchType Either \code{bestFirst} or \code{Exhaustive}
#' @export
#' @examples
#' form <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
#' rxCFS(form, data = iris)
rxCFS <- function(formula, data, searchType = c("bestFirst", "exhaustive")) {
  if (length(formula) != 3) {
    stop("Dependent and Independent Variables Are Required in the Formula")
  }
  var_info <- rxGetVarInfo(data)
  x_vars <- all.vars(formula)
  y <- x_vars[1]
  y_type <- var_info[[y]][["varType"]]
  y_numeric <- y_type %in% c("integer", "numeric")
  x_vars <- x_vars[-1]
  x_var_type <- unlist(lapply(var_info[x_vars], "[[", "varType"))
  if (!all(x_var_type %in% c("integer", "numeric"))) {
    stop("Independent Variables must be Numeric")
  }
  
  form <- as.formula(paste("~", paste(c(y,x_vars), collapse = "+")))
  all_cor <- rxCor(form, data = data)
  y_vars <- regexpr("=", row.names(all_cor)) - 1
  y_vars[y_vars < 0] <- nchar(row.names(all_cor))[y_vars < 0]
  y_vars <- substr(row.names(all_cor), 1, y_vars) == y
  feature_class <- if (y_numeric) {
    all_cor[-1, y]
  } else {
    y_prop <- as.matrix(prop.table(rxCrossTabs(as.formula(paste("~",y)), data = data, returnXtabs = TRUE)))
    t(matrix(y_prop)) %*% all_cor[y_vars, !y_vars]
  }
  all_cor <- all_cor[!y_vars, !y_vars]
  evaluator <- function(sel) {
    k <- length(sel)
    merit <- mean(feature_class[sel]) / sqrt(k + (k-1) * mean(all_cor[sel, sel]))
  }
  # Best First search
  bestFirst <- function(x) {
    evaluator <- function(sel) {
      k <- length(sel)
      merit <- sum(feature_class[sel]) / sqrt(k + (k-1) * sum(all_cor[sel, sel]))
    }
    # Initialize
    k <- length(x)
    best <- list()
    # Steps
    for (i in 1:k) {
      if (i == 1) {
        choices <- x
      } else {
        choices <- lapply(x[!(x %in% as.character(best[[i-1]][,-i]))], function(x) c(as.character(best[[i-1]][,-i]), x))
      }
      scores <- sapply(choices, evaluator)
      best_order <- order(scores, decreasing = TRUE)
      scores <- scores[best_order]
      choices <- choices[best_order]
      #best[[i]] <- choices[[1]]
      best[[i]] <- data.frame(Var = rbind(choices[[1]]), Merit = scores[1], stringsAsFactors = FALSE)
    }
    names(best[[1]])[1] <- "Var.1"
    best
  }
  # Greedy forward search
  bestFirst(x_vars)
}



