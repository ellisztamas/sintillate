#' Project samples onto a reference vector
#'
#' Project vectors of resampled x and y coordinates, for example from
#' bootstrapping or a Bayesian posterior distribution, and project them onto a
#' reference vector.
#'
#' @param samples Dataframe containing columns \code{x} and \code{y}, giving resampled
#' coordinates around a vector
#' @param ref Reference vector to project onto. Should be of length two, with an
#' element each for x and y coordinates. If NULL, the mean of x and y in
#' \code{samples} is used. Defaults to NULL.
#' @returns A vector of positions of each row in \code{samples} projected onto
#' \code{ref}.
#' @author Tom Ellis
#' @examples
#' \code{
#' library(sintillate)
#' library(MASS)
#' # To draw numbers from a multivariate normal
#' sample_mvnorm <- function(mu, sigma = matrix(c(1, 0,0, 1),2,2), n = 10000){
#'   draws <- as.data.frame(mvrnorm(n, mu, sigma))
#'   colnames(draws) <- c('x', 'y')
#'     draws
#'     }
#'
#' vec <- c(1,0) # reference vector
#' draws <- sample_mvnorm(vec, n = 10)
#' sin <- sintillate(draws$x, draws$y)
#'
#' # calculate what proportion overlap zero.
#' mean(z_star(sin) > 0)
#' }
z_star <- function(samples, ref=NULL){
  if( is.null(ref) ){
    ref <- c(
      mean(samples$x), mean(samples$y)
    )
  } else if( ! is.vector(ref) ){
    stop("`ref` should be blank, or a vector of length 2.")
  } else if( length(ref) != 2 ){
    stop("`ref` should be blank, or a vector of length 2.")
  }
  z_star <- as.matrix(samples[, c("x", "y")]) %*% ref / sqrt(ref[1]^2 + ref[2]^2)
  as.vector(z_star)
}
