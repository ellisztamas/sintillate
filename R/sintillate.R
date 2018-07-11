#' Quantify pleiotropic effects on two variables.
#'
#' \code{sintillate} calculates \eqn{\tau} and \eqn{q} for vectors \emph{x} and \emph{y}.
#'
#' @param x Value, or vector of values allongthe \emph{x}-axis.
#' @param y Value, or vector of values allongthe \emph{y}-axis.
#' @param SE_x,SE_y Optional standard errors for elements in \emph{x} and \emph{y}.
#'
#' @return Data frame including input data, vector norm angle, and \eqn{\tau}.
#' If standard errors are supplied, standard errors for the norm, angle, are
#' provided, as well as \emph{q} assuming normality of errors..
#'
#' @export
sintillate <- function(x, y, SE_x=NULL, SE_y=NULL){
  # Norm of the vector.
  z    <- vector_norm(x,y)
  # the angle of the norm.
  rad <- angle360(x, y)

  if(is.numeric(SE_x) & is.numeric(SE_y)){
    # output data
    out     <- data.frame(x = x, y = y, # return input data
                          z, rad,
                          degrees = (rad * 180) / (pi), # express angles in degrees.
                          tau     = tau(rad),           # calculate sine of twice the angle
                          # Standard errors
                          SE_x,
                          SE_y,
                          SE_z   = SE_norm( x, y, SE_x, SE_y, z),
                          SE_rad = SE_angle(x, y, SE_x, SE_y)
    )
    # q value assuming normality
    out$q = 1-pnorm(0, z, out$SE_z)*2

  } else {
    warning("Standard errors not provided, so calculation of q has been skipped.")
    out     <- data.frame(effectx = x, effecty = y, # return input data
                          norm, rad,
                          degrees = (rad * 180) / (pi), # express angles in degrees.
                          tau     = tau(rad) # calculate sine of twice the angle
    )
  }
  return(out)
}

