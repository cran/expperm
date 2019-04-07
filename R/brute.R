#' @title Brute-force calculation of an expected permutation matrix
#' @description Computes an expected permutation matrix and marginal likelihood from a matrix of assignment likelihoods. The function literally enumerates all permutations so will be impractial for matrices with more than 10 rows.
#' @param A A matrix of assignment likelihoods.
#' @return \code{E(P)}, the expected permutation matrix corresponding to \code{A}.
#' @examples
#' data(A)
#' brute(A)
brute<-function(A){
  n<-nrow(A)
  cnt<-rep(0,n)
  ind<-1:n
  indmat<-cbind(1:n,ind)
  W<-prod(A[indmat])
  EP<-diag(n)
  
  i<-0
  while(i<n){
    if(cnt[i+1]<i){                                           # The outer parts of this while
    if(i%%2==0){ind[c(1,i+1)]<-ind[c(i+1,1)]                  # loop implements Heap's algorithm
    }else{ind[c(i+1,cnt[i+1]+1)]<-ind[c(cnt[i+1]+1,i+1)]}     # for enumerating permutations.
    cnt[i+1]<-cnt[i+1]+1
    i<-0
    
    indmat<-cbind(1:n,ind)    # This part of the code updates
    w<-prod(A[indmat])        # the running weighted mean of 
    W<-W+w                    # permutation matrices.
    EP<-EP*(1-w/W)
    EP[indmat]<-EP[indmat]+w/W
    
    }else{
      cnt[i+1]<-0
      i<-i+1
    }
  }
  attr(EP,"permanent")<-W
  EP
}
