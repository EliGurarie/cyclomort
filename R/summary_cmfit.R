#' Provide a short summary of cmfit (parameter estimates for periodic mortality curves) objects
#' 
#' @param object a cmfit object
#' @param ... (not implemented)
#' 
#' @return a list containing a short summary of the estimates for each parameter along with confidence intervals and AIC
#' 
#' @example examples/cyclomortFit_example.R
#' @export

summary.cmfit = function(object, ...) {
  result = list(model = paste0("Multi-seasonal hazard function fit with ", object$n.seasons, " seasons with periodicity ", object$period, ".\n\n"))
  result$estimates = object$estimates
  result$analysis = paste0("Log-likelihood: ", round(object$logLik, 4), "; AIC: ", round(object$AIC, 4))
  result
}

#' @export
print.cmfit = function(x, ...) {
  cat(paste0("Multi-seasonal hazard function fit with ", x$n.seasons, " seasons with periodicity ", x$period, ".\n\n"))
  print(x$estimates)
  cat(paste0("Log-likelihood: ", round(x$logLik, 3), "; AIC: ", round(x$AIC, 3), "\n"))
}
