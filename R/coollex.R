#' @title Computing combinations with the Coollex algorithm
#' @description Computes the next combination in a sequence using the Coollex algortihm. When computing the combinations of k elements taken from n elements, the algorithm should be initialized by setting \code{ind<-c(rep(1,k),rep(0,n-k))} and \code{attr(ind,"i1")<-attr(ind,"i2")<-k}.
#' @param ind A binary string encoding the selected elements from a set.
#' @param i1 An integer counter. This counter (and the next) are used to make this implementation of the coollex algorithm 'loopless'. Initial values should be assigned as explained in the \code{description} section above. Later values are provided from earlier iterations of the function.
#' @param i2 A second integer counter.
#' @return A binary string encoding the next combination.
#' @examples
#' n<-4
#' k<-2
#' indi<-c(rep(1,k),rep(0,n-k))
#' attr(indi,"i1")<-attr(indi,"i2")<-k
#' allinds<-indi
#' for(i in 2:choose(n,k)){
#'  indi<-coollex(indi,attr(indi,"i1"),attr(indi,"i2"))
#'  allinds<-rbind(allinds,indi)
#' }
#' allinds
coollex<-function(ind,i1,i2){
  if(i1<length(ind)){
    ind[i1]<-0
    ind[i2]<-1
    i1<-i1+1
    i2<-i2+1
    if(ind[i1]==0){
      ind[i1]<-1
      ind[1]<-0
      if(i2>2){
        i1<-2
      }
      i2<-1
    }
  }
  attr(ind,"i2")<-i2
  attr(ind,"i1")<-i1
  ind
}
