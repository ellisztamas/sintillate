#' Calculate the L2 norm of a 2-d vector
#' @param x,y vectors of values for the \emph{x} and \emph{y} dimensions
#' @return Vector of norms for each pair of values in \emph{x} and \emph{y}
vector_norm <- function(x, y){
  sqrt(x^2 + y^2)
}
