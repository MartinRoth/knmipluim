context("MovingWindowExtremes")

## TODO: Rename context
## TODO: Add more tests

test_that("Moving window extremes", {
  x <- c(1, 2, 3, 2, 5, 0, 2, 3, 8)
  expect_error(GetWindowExtremes(x, 2.5, "max"), "Error : is_integer(x = k) is not TRUE\n", fixed = TRUE)
  expect_error(GetWindowExtremes(x, "a", "max"), "Error : is_integer(x = k) is not TRUE\n", fixed = TRUE)
  expect_error(GetWindowExtremes(x, c(2L,3L), "max"), "Error : length(k) not equal to 1\n", fixed = TRUE)
  expect_error(GetWindowExtremes(x, 2L, "mean"), "Method mean not defined.")
  expect_error(GetWindowExtremes(x, 2L, "sqrt"), "Method sqrt not defined.")
  expect_equal(GetWindowExtremes(x, 2L, "max"), c(rep(3, 2), rep(5, 4), rep(8, 3)))
  expect_equal(GetWindowExtremes(x, 2L, "min"), c(rep(1, 3), rep(0, 5), 2))
})
