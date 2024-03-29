#' Quantify pleiotropic effects on two variables.
#'
#' \code{sintillate} calculates vector length, angle \eqn{\tau} for vectors
#' \emph{x} and \emph{y}.
#'
#' Calculate basic summary statistics about a pair of vectors.
#' This can also be used to calculate bootstrap values for each parameter by
#' providing matrices rather than vectors of input values.
#'
#' @param x,y Values along the \emph{x}- and \emph{y}-axes. This will usually be
#' single floats or vectors of floats of observed values. Alternatively, supply
#' matrices of floats to calculate tau for a sample of bootstrapped *x* and *y*
#' values, or values drawn from a posterior distribution in a Bayesian analysis.
#' In this case, matrices should have a row for every observation, and a column
#' for every bootstrap/posterior draw.
#'
#' @return Data frame including input data, vector norm angle, and \eqn{\tau}.
#' If matrices were supplied, this returns a list of these data.
#'
#' @export
sintillate <- function(x, y, calculate_zstar=FALSE, zstar_ref=NULL){
  if(is.vector(x) & is.vector(y)){
    if(length(x) != length(y)) stop(paste("Vector x has", length(x),"elements but y has", length(y)))
  } else {
    if(is.matrix(x) & is.matrix(y)){
      if(any(dim(x) != dim(y))){
        stop(paste("Matrix x has dimensions {", nrow(x),",", ncol(x),"} but y has dimensions {", nrow(y),",", ncol(y),"}.", sep=""))
      }
    if( calculate_zstar ){
      warning("calculate_zstar is not currently implemented for matrices.")
    }
    }
  }

  rad <- angle360(x,y) # calculate angles in radians
  output <- list(
    x       = x, # return input data
    y       = y,
    norm    = vector_norm(x,y), # vector length
    radians = rad,   # the angle of the norm in radians
    degrees = (rad * 180) / (pi), # express angles in degrees.
    tau     = tau(rad)
  )

  # If the input data were vectors, collapse output into a data.frame.
  if(is.vector(x) & is.vector(y)){
    output <- do.call('cbind', output)
    output <- as.data.frame(output)
    if( calculate_zstar ){
      if( is.null(zstar_ref) ) {
        zstar_ref <- c( mean(output$x), mean(output$y) )
      }
      output$zstar <- z_star(samples = output, ref = zstar_ref)
    }
  }
  return(output)
}
