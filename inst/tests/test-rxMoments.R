### Test rxMoments
rxOptions(reportProgress=0)

context("rxMoments")


#  ------------------------------------------------------------------------

test_that("rxMoments works with data frame",{
  stopifnot(require("e1071"))
  tst <- rxMoments(~Sepal.Length, iris)

  g1 <- skewness(iris$Sepal.Length, type=1)
  G1 <- skewness(iris$Sepal.Length, type=2)
  b1 <- skewness(iris$Sepal.Length, type=3)
  
  expect_equal(g1, tst[["skewness"]][["g1"]])
  expect_equal(G1, tst[["skewness"]][["G1"]])
  expect_equal(b1, tst[["skewness"]][["b1"]])
  
  g2 <- kurtosis(iris$Sepal.Length, type=1)
  G2 <- kurtosis(iris$Sepal.Length, type=2)
  b2 <- kurtosis(iris$Sepal.Length, type=3)
  expect_equal(g2, tst[["kurtosis"]][["g2"]])
  expect_equal(G2, tst[["kurtosis"]][["G2"]])
  expect_equal(b2, tst[["kurtosis"]][["b2"]])
  
})


