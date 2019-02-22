#' Gaussian parametric bootstrapping
#'
#' Generate a sample of parametric bootstrap samples for a vector of values with
#' associated standard errors.
#'
#' @param x Vector of parameter estimates.
#' @param se Vector of standard errors for each element in `x`.
#' @param R Number of samples to draw.
#'
#' @return A matrix of bootstrap samples with a row for each element in `x` and
#' `R` columns.
#'
#' @author Tom Ellis
#' @export
parametric_resample <- function(x, se, R){
  vals <- rnorm(R*length(x))
  vals <- matrix(vals, nrow=length(x))
  vals <- (vals * se) + x
  return(vals)
}
