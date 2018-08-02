#' Distribution of pleiotropic effects
#'
#' Bins values of \eqn{\tau} across pairs of observations so that a histogram can be created.
#'
#' \code{tau_distribution} groups values of \eqn{tau} over observations into bins
#' defined by \code{width}. Within each bin, values for \emph{q} for each
#' \eqn{\tau} are summed. The resulting sum can be interpreted as the
#' probability of observing a vector with \eqn{\tau} in that bin, accoutning for
#' the robustness of the estimate of \eqn{\tau}.
#'
#' @param tau Vector of \eqn{\tau} values.
#' @param tau Vector of \eqn{q} values.
#' @param width Width of each bin. Passed to \code{cut}. Since \eqn{\tau} can
#' only be between -1 and 1, it is meaningless to assign a width greater than
#' that interval.
#'
#' @return Data.frame listing bining levels, the midpoints for each bin, and the
#' sums of \emph{q}-values in each bin.
#'
#' @export
tau_distribution <- function(tau, q, width = 1/8){
  if(width > 2){
    stop("Width > 2. Since $\\tau$ can only be between -1 and 1, it is meaningless to assign a width greater than that interval.")
  }
  if(length(q) != length(tau)){
    stop("Length of q and tau do not match.")
  }
  breaks <- seq(-1, 1, width)
  bks    <- cut(tau, breaks)

  yvals <- tapply(q, bks, sum, na.rm=TRUE)
  yvals[is.na(yvals)] <- 0
  yvals <- yvals / sum(yvals)

  data.frame(interval = levels(bks), midpoint = breaks[-1]-width/2, density=yvals, row.names = NULL)
}

