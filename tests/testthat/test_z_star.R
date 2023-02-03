library(sintillate)
library(MASS)

set.seed(29)

sample_mvnorm <- function(mu, sigma = matrix(c(1, 0,0, 1),2,2), n = 10000){
  draws <- as.data.frame(mvrnorm(n, mu, sigma))
  colnames(draws) <- c('x', 'y')
  draws
}

test_that("Vectical vector returns a single value", {
  vec <- c(1,0)
  draws <- data.frame(x = 1, y = seq(-3,3, 0.1))
  sint <- sintillate(draws$x, draws$y)
  expect_true( all(z_star(sint, vec) == 1) )
})

test_that("Samples project onto a horizontal vector", {
  vec <- c(1,0)
  draws <- sample_mvnorm(vec, n = 1000)
  sint <- sintillate(draws$x, draws$y)

  expect_equal( cor(draws$y, z_star(sint, vec)), 0, tolerance = 0.05)
  expect_equal( cor(draws$x, z_star(sint, vec)), 1)
})

test_that("NAs propogate", {
  vec <- c(1,0)
  draws <- sample_mvnorm(vec, n = 10)
  sint <- sintillate(draws$x, draws$y)
  sint$x[1] <- NA
  expect_true(NA %in% z_star(sint, vec))
})

test_that("Using ref=NULL gives nearly the same answer as setting the correct reference", {
  vec <- c(1,0)
  draws <- sample_mvnorm(vec, n = 10000)
  sint<- sintillate(draws$x, draws$y)
  expect_equal(mean(z_star(sint, vec) - z_star(sint)), 0, tolerance = 0.01 )
})

test_that("When x and y are negative, zstar stays positive", {
  vec <- c(-2, -2)
  draws <- sample_mvnorm(vec, n = 10000)
  sint<- sintillate(draws$x, draws$y)
  expect_true(mean(z_star(sint, vec) > 0) > 0.99)
})

test_that("Returns NaN If the reference vector is (0,0)", {
  vec <- c(0,0)
  draws <- sample_mvnorm(vec, n = 10000)
  sint<- sintillate(draws$x, draws$y)
  expect_true(
    all(
      is.nan(z_star(sint, vec))
      )
    )
})
