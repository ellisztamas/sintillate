#' Bootstrap vector norm and pleiotropic effects
#'
#' Parametric bootstrapping of vector length, and \eqn{\tau} assuming normality
#' of errors.
#'
#' If \emph{x} and \emph{y} are estimated with normally-distributed errors
#' \eqn{\sigma_x} and \eqn{\sigma_y}, the joint bootstrap distribution can be
#' estimated by drawing pairs of values from \eqn{N(x, \sigma_x)} and
#' \eqn{N(y, \sigma_y)}. The length of each drawn vector and its \eqn{\tau} are
#' calculated, and confidence intervals drawn from their distribution.
#'
#' In addition, the bootstrap distribution of \eqn{\tau} \emph{across} pairs of
#' observations in \emph{x} and \emph{y} is calculated by summing distributions
#' for each individual pair. Note that in contras to \code{tau_distribution}
#' this does incorporate confidence in position by summing \emph{q}; uncertainty
#' about distribution shape is accounted for through the bootstrapping process.
#'
#' @inheritParams sintillate
#' @param ndraws Integer number of bootstrap samples to draw.
#' @param conf_interval Probability coverage of the interval.
#' @param width Bin width for calculating the bootstrap distribution of \eqn{\tau}.
#'
#' @return A list of two elements giving confidence intervals for (z) vector
#' length and (tau) \eqn{\tau} values for each pair of elements in \code{x} and
#' \code{y}.
#'
#' @export
resample_angle <- function(x, y, SE_x, SE_y, ndraws = 1000, conf_interval = 0.95, width = 1/8){
  if(conf_interval >=1 | conf_interval <=0) stop('Width of the confidence interval should be between 0 and 1.')
  # determine which quantiles return two-tailed confidence intervals
  quantiles <- c(0.5 - conf_interval / 2,
                 0.5 + conf_interval / 2)
  # draw parametric bootstraps for each observation.
  samplex <- rnorm(ndraws*length(x))
  samplex <- matrix(samplex, ncol = ndraws)
  samplex <- (samplex * SE_x) + x
  # samples for y axis
  sampley <- rnorm(ndraws*length(x))
  sampley <- matrix(sampley, ncol = ndraws)
  sampley <- (sampley * SE_x) + y

  # vector lengths and tau for each realisation.
  norm <- vector_norm(samplex, sampley)
  sint <- tau(angle360(samplex, sampley))

  # Calculate the bootstrap distribution of tau values.
  breaks <- seq(-1,1, width)
  bks    <- cut(sint, breaks)
  binvals <- tapply(sint, bks, length)
  binvals[is.na(binvals)] <- 0
  binvals <- binvals / sum(binvals)
  dist    <- data.frame(interval = levels(bks), midpoint = breaks[-1]-width/2, binvals, row.names = NULL)

  return(list(
    z   = t(apply(norm, 1, function(obj) quantile(obj, quantiles))),
    tau = t(apply(sint, 1, function(obj) quantile(obj, quantiles))),
    distribution = dist
  ))
}
