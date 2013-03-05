# Function to expand a . to represent all variables in an XDF (DF)

formulaExpand <- function(formula, data){
  expandPos <- length(formula)
  if (formula[[expandPos]] == ".") {
    formNames <- names(rxGetVarInfo(data))
    if (expandPos == 3) {
      formNames <- formNames[!formNames %in% as.character(formula[[2]])]
    }
    formNames <- paste(formNames, collapse = "+")
    formula <- as.formula(paste(ifelse(expandPos == 3, as.character(formula[[2]]), ""), "~", formNames), env = .GlobalEnv)
  }
  return(formula)
}