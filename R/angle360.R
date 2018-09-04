#' Calculate the angle (in radians) of the hypotenuse between vectors x and y relative to axis y.
#'
#' @param x,y Numerical vectors.
#' @return A vector of radian values.
angle360 <- function(x, y){
  if(length(x) != length(y)) stop("x and y are different lengths.")
  if(any(x==0 & y==0)) warning("x and y contain one or more pairs of values that are both zero. NaN returned.")

  # Assign each pair of coordinates to a quadrant.
  quadrant <- numeric(length(x))
  quadrant[x >= 0 & y >= 0] <- 1
  quadrant[x  < 0 & y >= 0] <- 2
  quadrant[x  < 0 & y  < 0] <- 3
  quadrant[x >= 0 & y  < 0] <- 4
  # calculate angles
  theta <- atan(abs(x)/abs(y)) # for absolute x and y values
  theta[quadrant == 2] <- pi*2 - theta[quadrant == 2] # correction for quadrant 2
  theta[quadrant == 3] <- pi   + theta[quadrant == 3] # correction for quadrant 3
  theta[quadrant == 4] <- pi   - theta[quadrant == 4] # correction for quadrant 4
  return(theta)
}
