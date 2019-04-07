#' @title The Brualdi-Gibson method for computing an expected permutation matrix
#' @description Computes the expected permutation matrix and marginal likelihood from a tridiagonal matrix of assignment likelihoods using the Brualdi-Gibson method.
#' @param A A tridiagonal matrix of assignment likelihoods.
#' @return \code{E(P)}, the expected permutation matrix corresponding to \code{A}.
#' @examples
#' data(triA)
#' BG(triA)
BG<-function(A){
  if(!is.tridiagonal(A)){
    warning("Input is not tridiagonal. This function only works for tridiagonal matrices!")
  }
  n<-nrow(A)
  
  Fmat<-A                     # The algorithm begins by computing two sequences
  for(i in 2:n){              # of permanents via the BG contractions.
    Fmat[i,i:n]<-Fmat[i-1,i-1]*Fmat[i,i:n]+Fmat[i,i-1]*Fmat[i-1,i:n]
  }
  f<-diag(Fmat)
  Bmat<-A
  for(i in (n-1):1){
    Bmat[i,1:i]<-Bmat[i+1,i+1]*Bmat[i,1:i]+Bmat[i,i+1]*Bmat[i+1,1:i]
  }
  b<-diag(Bmat)

  EP<-matrix(0,n,n)           # The permanents are used to compute the diagonal
  EP[1,1]<-A[1,1]*b[2]/b[1]   # of EP, the off-diagonals are determined by the
  EP[n,n]<-A[n,n]*f[n-1]/b[1] # sum-to-one constraints.
  if(n>2){for(i in 2:(n-1)){EP[i,i]<-A[i,i]*f[i-1]*b[i+1]/b[1]}}
  for(i in 1:(n-1)){ EP[i,i+1]<-EP[i+1,i]<-1-sum(EP[i,1:i])}

  attr(EP,"permanent")<-b[1]
  EP
}
