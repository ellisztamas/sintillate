context("sintillate function")

n <- 100
# for vectors
x <- rnorm(n)
y <- rnorm(n)
# # For matrices
xmat <- matrix(x, nrow=10)
ymat <- matrix(y, nrow=10)

test_that('sintillate returns either data frame, or list', {
  expect_is(sintillate(3, 4), "data.frame")
  expect_is(sintillate(x, y), "data.frame")
  expect_is(sintillate(xmat, ymat), "list")
})

test_that("sintillate fails if inputs are different sizes", {
  expect_error(sintillate(x, y[1:10]), "Vector x has 100 elements but y has 10")
  expect_error(sintillate(xmat, head(ymat)), "has dimensions")
})

test_that("sintillate throws a warning if all x and y values are positive", {
  expect_is(sintillate(abs(x), y), "data.frame")
  expect_is(sintillate(x, abs(y)), "data.frame")
  expect_warning(sintillate(abs(x), abs(y)), "both x and y are positive")
})
