% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sintillate.R
\name{sintillate}
\alias{sintillate}
\title{Quantify pleiotropic effects on two variables.}
\usage{
sintillate(x, y, calculate_zstar = FALSE, zstar_ref = NULL)
}
\arguments{
\item{x, y}{Values along the \emph{x}- and \emph{y}-axes. This will usually be
single floats or vectors of floats of observed values. Alternatively, supply
matrices of floats to calculate tau for a sample of bootstrapped *x* and *y*
values, or values drawn from a posterior distribution in a Bayesian analysis.
In this case, matrices should have a row for every observation, and a column
for every bootstrap/posterior draw.}
}
\value{
Data frame including input data, vector norm angle, and \eqn{\tau}.
If matrices were supplied, this returns a list of these data.
}
\description{
\code{sintillate} calculates vector length, angle \eqn{\tau} for vectors
\emph{x} and \emph{y}.
}
\details{
Calculate basic summary statistics about a pair of vectors.
This can also be used to calculate bootstrap values for each parameter by
providing matrices rather than vectors of input values.
}
