context("sintillate function")

test_that('sintillate returns either data frame, or list', {
  expect_is(sintillate(3, 4), "data.frame")

  n <- 100
  # for vectors
  x <- rnorm(n)
  y <- rnorm(n)
  expect_is(sintillate(x, y), "data.frame")
  # # For matrices
  xmat <- matrix(x, nrow=10)
  ymat <- matrix(y, nrow=10)
  expect_is(sintillate(xmat, ymat), "list")
})

test_that("sintillate fails if inputs are different sizes", {
  n <- 100
  # for vectors
  x <- rnorm(n)
  y <- rnorm(n)
  expect_error(sintillate(x, y[1:10]), "Vector x has 100 elements but y has 10")
  # For matrices
  xmat <- matrix(x, nrow=10)
  ymat <- matrix(y, nrow=20)
  expect_error(sintillate(xmat, ymat), "has dimensions")
})
