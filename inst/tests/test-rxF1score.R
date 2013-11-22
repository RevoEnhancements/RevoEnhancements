### Test F1 score
rxOptions(reportProgress=0)

context("F1 score")


#  ------------------------------------------------------------------------

test_that("rxF1score works with data frame",{
  dat <- data.frame(iris[, -5], Vers = iris$Species == "versicolor")
  fit <- rxLogit(Vers ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, dat)
  prd <- rxPredict(fit, dat)$Vers_Pred
  cmb <- data.frame(Vers=dat$Vers, Pred=prd)
  tst <- rxF1score("Vers", "Pred", cmb)

  expect_is(tst, "list")
  expect_equal(names(tst), c("precision", "recall", "trueNegRate", "accuracy", "F1"))
  expect_equal(unname(unlist(tst)), c(0.641025641025641, 0.5, 0.86, 0.74, 0.561797752808989))
})


#  ------------------------------------------------------------------------

test_that("rxF1score works with XDF",{
  dataFile <- tempfile(pattern = ".data", fileext = ".xdf")
  dat <- data.frame(iris[, -5], Vers = iris$Species == "versicolor")
  rxDataStep(dat, outFile=dataFile, rowsPerRead=50)
  fit <- suppressWarnings(
    rxLogit(Vers ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
            dat)
  )
  prd <- rxPredict(fit, dat)$Vers_Pred
  cmb <- data.frame(Vers=dat$Vers, Pred=prd)
  tst <- rxF1score("Vers", "Pred", cmb)
  
  expect_is(tst, "list")
  expect_equal(names(tst), c("precision", "recall", "trueNegRate", "accuracy", "F1"))
  expect_equal(unname(unlist(tst)), c(0.641025641025641, 0.5, 0.86, 0.74, 0.561797752808989))
})

