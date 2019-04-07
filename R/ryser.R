#' @title The Ryser method for computing an expected permutation matrix
#' @description Computes the expected permutation matrix and marginal likelihood from a matrix of assignment likelihoods using the Ryser method.
#' @param A A matrix of assignment likelihoods.
#' @return \code{E(P)}, the expected permutation matrix corresponding to \code{A}.
#' @examples
#' data(A)
#' ryser(A)
ryser<-function(A){
  n<-ncol(A)
  W<-0
  EP<-matrix(0,n,n)
  a<-rep(0,n)
  f<-0:n
  p<-(-1)^n
  while(f[1]<n){
    p<--p                             # This part of the algorithm is
    j<-f[1]                           # Knuth's algorithm L for generating
    f[1]<-0                           # a binary Gray code.
    f[j+1]<-f[j+2]                    
    f[j+2]<-j+1                       
    a[j+1]<-1-a[j+1]                  
    Ak<-A
    
    Ak[,a==0]<-0                      # This part of the algorithm is
    rs<-rowSums(Ak)                   # computing the terms of the incl/excl
    w<-p*prod(rs)                     # sum in the same way as Ryser's
    if(!(w==0)){                      # method for computing the permanent.
    Ak<-sweep(Ak,1,rs,FUN="/")
    W<-W+w
    EP<-EP+w/W*(Ak-EP)
    }
  }
#  attr(EP,"permanent")<-W
  EP
}
