#' Display cmfitlist objects as text
#' 
#' @param x a cmfitlist object
#' 
#' @return a table comparing the different periodic mortality models, comparing them using AIC as well as the parameter estimates
#' 
#' @example examples/getIdealPeakFit_example.R
#' @export

print.cmfitlist = function(x) {
  print(summary(x))
}