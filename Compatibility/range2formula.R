# Function to create a formula from a range on an XDF (DF)

range2formula <- function(range, data){
  formNames <- names(rxGetVarInfo(data))[range]
  formNames <- paste(formNames, collapse = "+")
  return(as.formula(paste("~", formNames))) 
}