#' Count the frequencies of elements in discrete blocks
#'
#' Discretise continuous values into blocks, and count the frequency in each
#' block.
#'
#' \code{hist_vals} applies \code{cut} to the vector, then counts the number of
#' instances of each group. Counts are normalised to sum to one.
#'
#' @param x Vector of floats
#' @param breaks Break rule passed to \code{cut}. Either an integer number of
#' cut points, or a vector of cut points. See \code{?cut} for details.
#' @return Vector of probability densities for each bin.
hist_vals <- function(x, breaks=seq(-1,1,1/8)){
  counts <- table(cut(x, breaks = breaks))
  counts[is.na(counts)] <- 0
  counts <- counts/ sum(counts)
  counts
}
