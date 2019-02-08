#' @title The Brualdi-Gibson method for computing an expected permutation matrix
#' @description Computes the expected permutation matrix and marginal likelihood from a tridiagonal matrix of assignment likelihoods using the Brualdi-Gibson method.
#' @param A A tridiagonal matrix of assignment likelihoods.
#' @return \code{E(P)}, the expected permutation matrix corresponding to \code{A}.
#' @examples
#' data(triA)
#' BG(triA)
BG<-function(A){
  if(!is.tridiagonal(A)){
    warning("Input is not tridiagonal.
          This function only works for tridiagonal matrices!")
  }
  n<-nrow(A)

  f<-contractor(A)
  b<-rev(contractor(A[n:1,n:1]))

  EP<-matrix(0,n,n)
  EP[1,1]<-A[1,1]*b[2]/b[1]
  EP[n,n]<-A[n,n]*f[n-1]/b[1]
  for(i in 2:(n-1)){ EP[i,i]<-A[i,i]*f[i-1]*b[i+1]/b[1] }
  for(i in 1:(n-1)){ EP[i,i+1]<-1-sum(EP[i,1:i]);EP[i+1,i]<-EP[i,i+1] }

  attr(EP,"permanent")<-b[1]
  EP
}
