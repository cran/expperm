% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brute.R
\name{brute}
\alias{brute}
\title{Brute-force calculation of an expected permutation matrix}
\usage{
brute(A, return.permanent = FALSE)
}
\arguments{
\item{A}{A matrix of assignment likelihoods.}

\item{return.permanent}{A logical value indicating whether the function should also return the permanent of \code{A}, which is then added to the output as an attribute.}
}
\value{
\code{E(P)}, the expected permutation matrix corresponding to \code{A}.
}
\description{
Computes an expected permutation matrix and marginal likelihood from a matrix of assignment likelihoods. The function literally enumerates all permutations so will be impractial for matrices with more than 10 rows.
}
\examples{
data(A)
brute(A)
}
