#' @title Brute-force calculation of an expected permutation matrix
#' @description Computes an expected permutation matrix and marginal likelihood from a matrix of assignment likelihoods. The function literally enumerates all permutations so will be impractial for matrices with more than 10 rows.
#' @param A A matrix of assignment likelihoods.
#' @return \code{E(P)}, the expected permutation matrix corresponding to \code{A}.
#' @examples
#' data(A)
#' brute(A)
brute<-function(A){
  n<-nrow(A)
  ind<-1:n
  W<-prod(A[cbind(1:n,ind)])
  EP<-diag(n)
  for(i in 2:factorial(n)){
    ind<-heap(ind)
    indmat<-cbind(1:n,ind)
    w<-prod(A[indmat])
    W<-W+w
    EP<-EP*(1-w/W)
    EP[indmat]<-EP[indmat]+w/W
  }
  attr(EP,"permanent")<-W
  EP
}
