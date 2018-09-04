#' Quantify pleiotropic effects on two variables.
#'
#' \code{sintillate} calculates \eqn{\tau} and \eqn{q} for vectors \emph{x} and \emph{y}.
#'
#' @param x,y Value, or vector of values allongthe \emph{x}- and \emph{y}-axes.
#'
#' @return Data frame including input data, vector norm angle, and \eqn{\tau}.
#'
#' @export
sintillate <- function(x, y){
  if(is.vector(x) & is.vector(y)){
    if(length(x) != length(y)) stop(paste("Vector x has", length(x),"elements but y has", length(y)))
  } else {
    if(is.matrix(x) & is.matrix(y)){
      if(any(dim(x) != dim(y))){
        stop(paste("Matrix x has dimensions {", nrow(x),",", ncol(x),"} but y has dimensions {", nrow(y),",", ncol(y),"}.", sep=""))
      }
    }
  }

  rad <-angle360(x, y)
  list(
    x = x, # return input data
    y = y,
    norm = vector_norm(x,y), # vector length
    rad,   # the angle of the norm in radians
    degrees = (rad * 180) / (pi), # express angles in degrees.
    tau     = tau(rad) # calculate sine of twice the angle
  )
}
