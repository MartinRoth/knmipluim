context("ReturnLevelClimatology")

library(knmipluim)

## TODO: Rename context
## TODO: Add more tests

test_that("Regression test", {
  maxDat <- tx260_1981_2010
  maxRetLevel <- ReturnLevelClimatology(maxDat, "tx", "max", kLoc = 15, kScale = 8, kShape = 4)
  minDat <- tn260_1981_2010
  minRetLevel <- ReturnLevelClimatology(minDat, "tn", "min", kLoc = 15, kScale = 8, kShape = 4)
  expect_equal_to_reference(maxRetLevel, "ReturnLevelMaxima.rds")
  expect_equal_to_reference(minRetLevel, "ReturnLevelMinima.rds")
})
