# Test F1 score
rxOptions(reportProgress=0)

context("F1 score")

test_that("rxF1score works with data frame",{
  dat <- data.frame(iris[, -5], Virg = iris$Species == "virginica")
  fit <- suppressWarnings(
    rxLogit(Virg ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                 dat)
  )
  prd <- rxPredict(fit, dat)$Virg_Pred
  cmb <- data.frame(Virg=dat$Virg, Pred=prd)
  tst <- rxF1score("Virg", "Pred", cmb)

  expect_is(tst, "list")
  expect_equal(names(tst), c("precision", "recall", "trueNegRate", "accuracy", "F1"))
  expect_equivalent(unname(unlist(tst)), c(0.98, 0.98, 0.99, 0.98666667, 0.98))
})


test_that("rxF1score works with XDF",{
  dataFile <- tempfile(pattern = ".data", fileext = ".xdf")
  dat <- data.frame(iris[, -5], Virg = iris$Species == "virginica")
  rxDataStep(dat, outFile=dataFile, rowsPerRead=50)
  fit <- suppressWarnings(
    rxLogit(Virg ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
            dat)
  )
  prd <- rxPredict(fit, dat)$Virg_Pred
  cmb <- data.frame(Virg=dat$Virg, Pred=prd)
  tst <- rxF1score("Virg", "Pred", cmb)
  
  expect_is(tst, "list")
  expect_equal(names(tst), c("precision", "recall", "trueNegRate", "accuracy", "F1"))
  expect_equivalent(unname(unlist(tst)), c(0.98, 0.98, 0.99, 0.98666667, 0.98))
})

