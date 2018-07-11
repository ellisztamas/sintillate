#' Calculate the L2 norm of a 2-d vector
#' @param x,y vectors of values for the \emph{x} and \emph{y} dimensions
#' @return Vector of norms for each pair of values in \emph{x} and \emph{y}
vector_norm <- function(x, y){
  sqrt(x^2 + y^2)
}

#' Standard error of the L2 norm of a vector.
#'
#' Calculate the standard error of the L2 norm for vectors of pairs of
#' coordinates.
#'
#' @param x,y vectors of values for the \emph{x} and \emph{y} dimensions
#' @param SE_x,SE_y Vectors of standard errors for \emph{x} and \emph{y}
#' @param norm Vector of L2 norms between \emph{x} and \emph{y} (hypotenuse between *x*
#' and *y*)
#' @return Vector of norms for each pair of values in \emph{x} and \emph{y}
SE_norm <- function(x, y, SE_x, SE_y, norm){
  2*norm * sqrt((2* x / SE_x)^2 + (2* y / SE_y)^2)
}

#\deqn{2z \sqrt{\frac{2x}{\sigma_x}^2 + \frac{2y}{\sigma_y}^2}}

#' Convert radian angles to degrees.
#' @param a vector of angles in radians.
#' @return A vector of angles in degrees.
degrees <- function(a) (a * 180) / (pi)

#' Standard error of an angle
#'
#' Calculate the standard error of an acute angle in a right-angle triangle.
#'
#' @param x,y vectors of values for the \emph{x} and \emph{y} dimensions
#' @param SE_x,SE_y Vectors of standard errors for \emph{x} and \emph{y}
#' @return Vector of standard errors for \eqn{\theta}.
SE_angle  <- function(x, y, SE_x, SE_y){
  # For a right angle triangle with sides \emph{x} and \emph{y} with standard errors
  # {\eqn{\sigma_{x}, \sigma_{y}}, angle \eqn{\theta} is \eqn{ \tan (y/x) }, with
  # standard error:
  # \deqn{\sqrt{\frac{(\sigma_x /x)^2 + (\sigma_y/y)^2}{1+(x/y)^2}} }
  sqrt((SE_x/effectx)^2 + (SE_y/effecty)^2) / (1+ (effecty/effectx)^2)
}

#' Quantify pleiotropy between two variables.
#'
#' \code{tau} calculates the statistic \eqn{\tau} between two data points.
#'
#' @param a Vector of angles in radians.
#' @return Vector of \eqn{\tau} values between -1 and 1.
tau <- function(a) asin(sin(2*a))/ asin(1)
