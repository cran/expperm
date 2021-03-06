% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BG.R
\name{BG}
\alias{BG}
\title{The Brualdi-Gibson method for computing an expected permutation matrix}
\usage{
BG(A, return.permanent = FALSE)
}
\arguments{
\item{A}{A tridiagonal matrix of assignment likelihoods.}

\item{return.permanent}{A logical value indicating whether the function should also return the permanent of \code{A}, which is then added to the output as an attribute.}
}
\value{
\code{E(P)}, the expected permutation matrix corresponding to \code{A}.
}
\description{
Computes the expected permutation matrix and marginal likelihood from a tridiagonal matrix of assignment likelihoods using the Brualdi-Gibson method.
}
\examples{
data(triA)
BG(triA)
}
