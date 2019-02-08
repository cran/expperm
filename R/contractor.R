#' @title Contract a tridiagonal matrix
#' @description An internal function employed by the BG method for computing the permanent.
#' @param A A tridiagonal matrix of order n x n.
#' @return A tridiagonal matrix of order (n-1) x (n-1) with the same permanent as \code{A}.
#' @examples
#' data(A)
#' contractor(A)
contractor<-function(A){
  n<-nrow(A)
  f<-c();f[1]<-A[1,1]
  for(its in 2:n){
    A[2,]<-A[1,1]*A[2,]+A[2,1]*A[1,]
    A<-A[-1,-1]
    f[its]<-A[1]
  }
  f
}
