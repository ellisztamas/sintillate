context("Functions for plotting the distribition of tau")

# generate sample tau around 0.1
n <- 100
x <- rnorm(n, mean = 0.1, 0.2)
# generate a matrix of bootstrap values by adding gaussian noise to each value
nreps <- 1000
xbs <- rnorm(n * nreps, 0, 0.02)
xbs <- matrix(xbs, nrow = n)
xbs <- x + xbs

test_that("tau_distribuition returns correct dimensions", {
  expect_equal(dim(tau_distribution(x)),      c(16,2))
  expect_equal(dim(tau_distribution(x, xbs)), c(16,4))
})

test_that("Mean tau values fall beyond condidence intervals", {
  td <- tau_distribution(x, xbs)
  expect_true(all(td$density <= td$upper_CI))
  expect_true(all(td$density >= td$lower_CI))
})

test_that("tau_distribution fails when shape of bootstraps does not match observed data.",{
  expect_error(tau_distribution(x, xbs[-1,]))
})

test_that("tau_distribution fails when input values are beyond {-1, 1}.", {
  expect_error(tau_distribution(rnorm(1000)), "greater than 1")
  expect_error(tau_distribution(x, xbs*2), "greater than 1")
})
