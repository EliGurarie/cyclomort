#' Determine log-likelihood for a cmfit object
#' 
#' @param x a cmfit object
#' 
#' @return log-likelihod for the cmfit object to be used for comparison with other fits
#' 
#' @example examples/cyclomortFit_example.R
#' 
#' @export

logLik.cmfit = function(x) {
  x$logLik
}