#' Print a short summary of cmfit (parameter estimates for periodic mortality curves) objects
#' 
#' @param x a cmfit object
#' @param maxDigits number of digits to round to
#' 
#' @return a short summary of the estimates for each parameter along with confidence intervals and AIC
#' 
#' @example examples/cyclomortFit_example.R
#' @export

print.cmfit = function(x, maxDigits = 4) {
  cat(paste0("Multi-seasonal hazard function fit with ", n.seasons, " seasons with periodicity ", period, ".\n\n"))
  print(x$estimates)
  cat(paste0("Log-likelihood: ", round(x$logLik, maxDigits), "; AIC: ", round(x$AIC, maxDigits), "\n"))
}
