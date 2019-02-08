#' @title The Ryser method for computing an expected permutation matrix
#' @description Computes the expected permutation matrix and marginal likelihood from a matrix of assignment likelihoods using the Ryser method.
#' @param A A matrix of assignment likelihoods.
#' @return \code{E(P)}, the expected permutation matrix corresponding to \code{A}.
#' @examples
#' data(A)
#' ryser(A)
ryser<-function(A){
  m<-nrow(A)
  n<-ncol(A)
  W<-0
  EP<-matrix(0,m,n)
  for(k in 0:(m-1)){
    ind<-c(rep(1,k),rep(0,n-k))
    attr(ind,"i1")<-attr(ind,"i2")<-k
  for(i in 1:choose(n,k)){
      Ak<-A
      Ak[,as.logical(ind)]<-0
      rs<-rowSums(Ak)
      w<-(-1)^k*choose(n-m+k,k)*prod(rs)
      if(!w==0){
        W<-W+w
        EP<-EP+w/W*(sweep(Ak,1,rs,FUN="/")-EP)
      }
    ind<-coollex(ind,attr(ind,"i1"),attr(ind,"i2"))
  }}
  attr(EP,"permanent")<-W
  EP
}
