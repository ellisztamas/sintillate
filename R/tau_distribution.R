#' Distribution of pleiotropic effects
#'
#' Bins values of \eqn{\tau} across pairs of observations so that a histogram
#' can be created.
#'
#' \code{tau_distribution} groups values of \eqn{tau} over observations into bins
#' defined by \code{width}. Within each bin, values for \emph{q} for each
#' \eqn{\tau} are summed. In addition, a matrix of \eqn{tau} values can be given to estimate confidence
#' intervals within each bin.
#'
#' @param tau Vector of \eqn{\tau} values.
#' @param bootstrap Optional matrix of \eqn{tau} values derived from bootstrapping
#' the *x* and *y* values used to create vector \code{tau}. This should have a row
#' for every element in \code{tau}.
#' @param width Width of each bin passed to \code{cut}. Since \eqn{\tau} can
#' only be between -1 and 1, it is meaningless to assign a width greater than
#' that interval.
#'
#' @return Data.frame listing midpoints for each bin, and density within each
#' bin. If bootstrapped values of \eqn{tau} are provided, upper and lower
#' confidence intervals are returned as well.
#'
#' @export
tau_distribution <- function(tau, bootstrap=NULL, width = 1/8){
  if(width > 1){
    stop("Width > 1. Since $\\tau$ can only be between -1 and 1, it is meaningless to split values into blocks greater than 1.")
  }
  # Check input value all fall between -1 and 1.
  if(any(tau >  1)) stop("One or more values of tau are greater than 1.")
  if(any(tau < -1)) stop("One or more values of tau are less than 1.")

  breaks <- seq(-1, 1, width)
  yvals <- as.numeric(hist_vals(tau, breaks))

  if(is.null(bootstrap)){
    out <- data.frame(
      midpoint = breaks[-1]-width/2,
      density  = yvals)
  } else {
    if(nrow(bootstrap) != length(tau)){
      stop("Matrix of bootstrap values has", nrow(bootstrap), "rows, but the original dataset has", nrow(tau))
    }
    # Check bootstrap values all fall between -1 and 1.
    if(any(bootstrap >  1)) stop("One or more values in bootstrap are greater than 1.")
    if(any(bootstrap < -1)) stop("One or more values in bootstrap are less than 1.")

    boot_cut <- apply(bootstrap, 2, hist_vals, breaks=breaks)
    out <- data.frame(
      midpoint = breaks[-1]-width/2,
      density  = yvals,
      mean     = apply(boot_cut, 1, mean),
      lower_CI = apply(boot_cut, 1, quantile, 0.025),
      upper_CI = apply(boot_cut, 1, quantile, 0.975),
      row.names = NULL
    )
  }
  return(out)
}
