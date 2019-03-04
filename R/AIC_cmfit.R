#' Determine AIC for a cmfit object
#' 
#' @param x a cmfit object
#' 
#' @return AIC (a measure of model accuracy) for the cmfit object
#' 
#' @example examples/cyclomortFit_example.R
#' 
#' @export

AIC.cmfit = function(x) {
  x$AIC
}