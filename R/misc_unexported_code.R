###################################
# package handlers
###################################

#' @useDynLib align
#' @importFrom Rcpp sourceCpp

#' @importFrom stats approx cor
#' @importFrom utils capture.output methods

#undefined globals
utils::globalVariables(c("index", "scores", "warp.y", "iteration"))

##################################
#cow_convert
##################################

#kr v.0.0.1

#converts cow outputs nSeg and BT
#into something that can be used
#by warp_frame

#probably not staying or not staying as is...

cow_convert <- function(nSeg, bT){
  bT <- as.vector(bT)
  d <- diff(nSeg)

  ans <- c(nSeg[1])
  for(i in 1:length(d)){
    ans <- c(ans,
             seq(bT[i]+1,bT[i+1], length.out=d[i]))
  }

  data.frame(x=min(nSeg):max(nSeg),
             y=ans)
}
