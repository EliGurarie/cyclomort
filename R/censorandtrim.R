#' Censor and Trim
#' 
#' Functions for right-censoring and left-trimming survival data. They are convenient
#' for comparing cyclomort fits before and after some cut-off time, as in the example
#' below. 
#' 
#' @param x cycloSurv object
#' @param censor.time time of (right) censoring, or vector of times of censoring
#' @return Censored Surv object
#' @example examples/fittingWAH.R
#' @export

censor_cycloSurv <- function(x, censor.time){
  
  if(is.numeric(censor.time)) 
    ct <- censor.time else 
      ct <- as.numeric(difftime(censor.time,  attributes(x)$t0, units = attributes(x)$timeunits))
  
  if(length(ct) == 1)
    cts <- rep(ct, length(x)) else 
      if(length(ct) == length(x))
        cts <- ct else stop("censor.time must be equal to 1 or length(x).")
  
  keep <- x[,1] < cts
  event <- x[,3] & !(x[,2] > cts)
  end <- pmin(x[,2], cts)
  
  x.censored <- create_cycloSurv(start = x[keep,1], end =  end[keep], 
                                 event = event[keep], period = attributes(x)$period, 
                                 timeunits = attributes(x)$timeunits)
  
  attributes(x.censored)$t0 <- attributes(x)$t0
  x.censored
}

#' @param trim.time time of (left) trimming
#' @return Trimmed Surv object
#' @rdname censor_cycloSurv
#' @export
#' 
trim_cycloSurv <- function(x, trim.time){
  
  if(is.numeric(trim.time)) 
    tt <- trim.time else 
      tt <- as.numeric(difftime(trim.time,  attributes(x)$t0, units = attributes(x)$timeunits))
    
  x.trimmed <- x[x[,2] > tt]
  x.trimmed[,1][x.trimmed[,1] < tt] <- tt
  
  class(x.trimmed) <- class(x)
  attributes(x.trimmed)$t0 <- attributes(x)$t0
  attributes(x.trimmed)$period <- attributes(x)$period
  attributes(x.trimmed)$timeunits <- attributes(x)$timeunits
  
  x.trimmed
}
