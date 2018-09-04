#' Quantify tau between two variables.
#'
#' \code{tau} calculates the statistic \eqn{\tau} between two data points.
#'
#' @param a Vector of angles in radians.
#' @return Vector of \eqn{\tau} values between -1 and 1.
tau <- function(a){
  asin(sin(2*a))/ asin(1)
}
