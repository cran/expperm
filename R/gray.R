#' @title Gray code enumeration
#' @description Computes the next binary string in a Gray code sequence using Knuth's Algorithm G.
#' @param ind A binary string.
#' @return The next binary string in the Gray code sequence.
#' @examples
#' n<-4
#' indi<-rep(0,n)
#' allinds<-c()
#' for(i in 1:(2^n)){
#'   allinds<-rbind(allinds,indi)
#'   indi<-gray(indi)
#' }
#' allinds
gray<-function(ind){
  p<-sum(ind)%%2
  if(p==0){j<-1}else{j<-which.max(ind)+1}
  if(j<=length(ind)){ind[j]<-1-ind[j]}
  ind
}
