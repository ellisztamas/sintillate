#' Bootstrap vector norm and pleiotropic effects
#'
#' Parametric bootstrapping of vector length, and \eqn{\tau} assuming normality
#' of errors.
#'
#' @inheritParams sintillate
#' @param ndraws Integer number of bootstrap samples to draw.
#' @param conf_interval Probability coverage of the interval.
#'
#' @return A list of two elements giving confidence intervals for (z) vector
#' length and (tau) \eqn{\tau} values for each pair of elements in \code{x} and
#' \code{y}.
#'
#' @export
resample_angle <- function(x, y, SE_x, SE_y, ndraws, conf_interval = 0.95){
  if(conf_interval >=1 | conf_interval <=0) stop('Width of the confidence interval should be between 0 and 1.')
  # determine which quantiles return two-tailed confidence intervals
  quantiles <- c(0.5 - conf_interval / 2,
                 0.5 + conf_interval / 2)
  # draw parametric bootstraps for each observation.
  samplex <- rnorm(ndraws*length(xvals))
  samplex <- matrix(samplex, ncol = ndraws)
  samplex <- (samplex * SE_x) + xvals
  # samples for y axis
  sampley <- rnorm(ndraws*length(xvals))
  sampley <- matrix(sampley, ncol = ndraws)
  sampley <- (sampley * SE_x) + yvals

  # vector lengths and tau for each realisation.
  norm <- vector_norm(samplex, sampley)
  sint <- tau(angle360(samplex, sampley))

  return(list(
    z   = t(apply(norm, 1, function(obj) quantile(obj, quantiles))),
    tau = t(apply(sint, 1, function(obj) quantile(obj, quantiles)))
  ))
}
