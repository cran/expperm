#' @title Heap's algorithm for enumerating permutations
#' @description Computes the next permutation in a sequence that eventually includes, without repetition, all possible permutations. There are many R packages with functions for computing all permutations, most of which are more highly optimized than \code{heap}. The value of the current implementation is its relative transparency and `hack-ability', which helps us use it for the brute-force enumeration of expected permutation matrices.
#' @param ind A permuted vector of the integers $\{1,\ldots,n\}$ for some $n$. For all terms in the sequence other than the first, the input is vector is expected to include a `counter' attribute, which is needed by the algorithm to keep track of its position.
#' @return The next permutation in the Heap sequence, accompanied by an updated counter attribute.
#' @examples
#' n<-4
#' permi<-1:n
#' allperms<-c()
#' for(i in 1:factorial(n)){
#'   allperms<-rbind(allperms,permi)
#'   permi<-heap(permi)
#' }
#' allperms
heap<-function(ind){

  cnt<-attr(ind,"cnt")
  if(is.null(cnt)){cnt<-rep(0,length(ind))}
  n<-length(ind)

  i<-0
  while(cnt[i+1]>=i & i<n){
    cnt[i+1]<-0
    i<-i+1
  }

  if(i%%2==0){ind[c(i+1,1)]<-ind[c(1,i+1)]}
  if(i%%2==1){ind[c(i+1,cnt[i+1]+1)]<-ind[c(cnt[i+1]+1,i+1)]}
  cnt[i+1]<-cnt[i+1]+1

  attr(ind,"cnt")<-cnt
  ind

}
