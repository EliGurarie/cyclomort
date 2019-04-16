#' Censor and Trim
#' 
#' Internal functions for right-censoring and left-trimming survival data.
#' 
#' @param x cycloSurv object
#' @param censor.times time of  (right) censoring, or vector of times of censoring
#' @return Censored / trimmed Surv object
#' @export

censor.Surv <- function(x, censor.times){
  x2 <- x
  cts <- rep(censor.times, length(x)/length(censor.times))
  x2[,2][x2[,2]>cts] <- cts[x2[,2]>cts]
  x2[,3][x2[,2]==cts] <- 0
  x2
}

#' @export
trim.Surv <- function(x, trim.times){
  tts <- rep(trim.times, length(x)/length(trim.times))
  x[x[,1]>tts,]
}
