#Function to calculate the KS Statistic

rxKS <- function (x) {
  UseMethod("rxKS")
}

rxKS.rxLorenz <- function (x) {
  max(abs(x$percents - x$cumVals)) / 100
}