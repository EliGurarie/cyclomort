#' Provide a short summary of cmfit (parameter estimates for periodic mortality curves) objects
#' 
#' @param x a cmfit object
#' 
#' @return a list containing a short summary of the estimates for each parameter along with confidence intervals and AIC
#' 
#' @example examples/cyclomortFit_example.R
#' @export

summary.cmfit = function(x) {
  result = list(model = paste0("Multi-seasonal hazard function fit with ", x$n.seasons, " seasons with periodicity ", x$period, ".\n\n"))
  result$estimates = x$estimates
  result$analysis = paste0("Log-likelihood: ", round(x$logLik, 4), "; AIC: ", round(x$AIC, 4))
  result
}