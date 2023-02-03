#' Calculate the angle (in radians) of the hypotenuse between vectors x and y relative to axis y.
#'
#' @param x,y Numerical vectors.
#' @return A vector of radian values.
angle360 <- function(x, y){
  if(length(x) != length(y)) stop("x and y are different lengths.")

  # Angle from the positive x axis
  theta <- atan2(y,x)
  # For angles below the y axis have these continue
  theta[ theta<0 ] <- 2*pi + theta[ theta<0 ]
  # Set zero vectors to NaN, because this is undefined.
  if(any(x==0 & y==0)) warning("x and y contain one or more pairs of values that are both zero. NaN returned.")
  theta[x == 0 & y == 0] <- NaN

  return(theta)
}
