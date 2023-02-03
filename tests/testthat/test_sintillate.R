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


test_that("calculate_zstar works for vectors", {
  n <- 1000
  # for vectors
  x <- rnorm(n, mean = 1)
  y <- rnorm(n, mean= 1 )
  # Set the correct vector direction
  sint <- sintillate(x,y, calculate_zstar = TRUE, zstar_ref = c(1,1))
  expect_gt( cor(sint$zstar, sint$y), 0.5 )
  expect_gt( cor(sint$zstar, sint$x), 0.5 )
  # Let it guess the vector.
  sint <- sintillate(x,y, calculate_zstar = TRUE)
  expect_gt( cor(sint$zstar, sint$y), 0.5 )
  expect_gt( cor(sint$zstar, sint$x), 0.5 )
})
