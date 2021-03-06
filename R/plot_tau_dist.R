#' Plot the distribution of \eqn{\tau}
#'
#' Plot a histogram of the distibution of \eqn{\tau}, overplotted with bootstrap means
#' and confidence intervals for each bin.
#'
#' @param tau Vector of observed values of \eqn{\tau}.
#' @param bootstraps Matrix of bootstrapped values of \eqn{\tau}. This is usually
#' generated by calling \code{sintillate} on matrices of *x* and *y* values. See
#' \code{?sintillate} for more.
#' @param width Width of the bars. Defaults to 1/8.
#' @param length Length of the horizontal ends of the error bars.
#' @param ... Further arguments passed to barplot.
#'
#' @export
plot_tau_dist <- function(tau, bootstraps, width=1/8, length=0.05, ...){
  # summarise bar heights and error bars
  td <- tau_distribution(tau, bootstraps,  width)
  # Plot bars heights
  barplot(td$density, space = 0, ylim=c(0, max(td$upper_CI)), ...)
  axis(1, at = 0:4 * (nrow(td)/4), labels =seq(-1, 1, 1/2))

  # add error bars and points
  arrows(1:nrow(td)-0.5, y0 = td$upper_CI,
         1:nrow(td)-0.5, y1 = td$lower_CI,
         angle = 90, code = 3, length = length
  )
  points(1:nrow(td)-0.5, td$mean, col='black', type='o', pch=21, bg='white')

}
