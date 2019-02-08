#' @title A variational approximation of an expected permutation matrix
#' @description Computes an approximate expected permutation matrix and marginal likelihood from a matrix of assignment likelihoods. The approximation minimizes a constrained KL divergence from the likelihood, and is computed via the repeated renormalization of the input's rows and columns.
#' @param A A matrix of assignment likelihoods.
#' @param maxit An integer specifying the maximum number of steps used in the optimization.
#' @return \code{E(P)}, the expected permutation matrix corresponding to \code{A}.
#' @examples
#' data(A)
#' sink(A)
sink<-function(A,maxit=99){
  n<-nrow(A);u<-rep(1,n);v<-rep(1,n)
  its<-0;rsums<-0
  while( its<maxit & max(abs(rsums-1))>1e-5 ){
    its<-its+1
    rsums<-rowSums(A);u<-rsums*u
    A<-sweep(A,1,rsums,FUN="/")
    csums<-colSums(A);v<-csums*v
    A<-sweep(A,2,csums,FUN="/")
  }
  W<-prod(u,v)
  attr(A,"permanent bound")<-W
  A
}
