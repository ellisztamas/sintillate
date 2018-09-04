context('Vector functions')

test_that('Vector functions give the same answer for vectors as for matrices', {
  # Generate data
  n <- 100
  x <- rnorm(n)
  y <- rnorm(n)
  # x and y as matrices
  xmat <- matrix(x, ncol=10)
  ymat <- matrix(y, ncol=10)

  expect_equal(vector_norm(x,y), as.vector(vector_norm(xmat, ymat)))
  expect_equal(angle360(x, y),   as.vector(angle360(xmat, ymat)))
  expect_equal(tau(angle360(x,y)), as.vector(tau(angle360(xmat, ymat))))
})

test_that("vector_norm and angle360 for a pair of floats.", {
  expect_equal(vector_norm(3,4), 5)
  expect_equal(angle360(0,-4), pi)
})

test_that("Tau returns correct value for eight points", {
  val <- tau(seq(0, 2*pi, pi/8))
  out <- c(0, 0.5, 1, 0.5, 0, -0.5, -1, -0.5, 0, 0.5, 1, 0.5,0, -0.5, -1, -0.5, 0)
  expect_equal(round(val,2), out)
})

test_that("angle360 throws NaN for zero-length vectors", {
  x <- 0:2
  y <- 0:2
  expect_warning(angle360(x, y), "x and y contain one or more pairs of values that are both zero. NaN returned.")
  expect_equal(angle360(x,y), c(NaN, pi/4, pi/4))
})
