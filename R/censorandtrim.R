#' Censor and Trim
#' 
#' Internal functions for right-censoring and left-trimming survival data.
#' 
#' @param x cycloSurv object
#' @param censor.times time of  (right) censoring, or vector of times of censoring
#' @return Censored / trimmed Surv object
#' @export

censor.cycloSurv <- function(x, censor.times){
  x2 <- x
  cts <- rep(censor.times, length(x2[,1])/length(censor.times))
  x2[,2][x2[,2]>cts] <- cts[x2[,2]>cts]
  x2[,3][x2[,2]==cts] <- 0
  x2
}

#' @export
trim.cycloSurv <- function(x, trim.times){
  tts <- rep(trim.times, length(x[,1])/length(trim.times))
  x[x[,1]>tts,]
}
