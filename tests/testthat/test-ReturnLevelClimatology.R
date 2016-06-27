context("ReturnLevelClimatology")

library(knmipluim)

## TODO: Rename context
## TODO: Add more tests

test_that("Regression test", {
  maxDat <- txDat[name == "tx260_1981_2010"]
  maxDat[, name := NULL]
  maxRetLevel <- ReturnLevelClimatology(maxDat, "tx", "max", kLoc = 15, kScale = 8, kShape = 4)
  minDat <- tnDat[name == "tn260_1981_2010"]
  minDat[, name := NULL]
  minRetLevel <- ReturnLevelClimatology(minDat, "tn", "min", kLoc = 15, kScale = 8, kShape = 4)
  expect_equal_to_reference(maxRetLevel, "ReturnLevelMaxima.rds")
  expect_equal_to_reference(minRetLevel, "ReturnLevelMinima.rds")
})