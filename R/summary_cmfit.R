#' Provide a short summary of cmfit (parameter estimates for periodic mortality curves) objects
#' 
#' @param object a cmfit object
#' @param date logical dictating whether peaks of high mortality are expressed as Dates
#' @param ... (not implemented)
#' 
#' @return a list containing a short summary of the estimates for each parameter along with confidence intervals and AIC
#' 
#' @example examples/cyclomortFit_example.R
#' @export

summary.cmfit = function(object, date = FALSE, ...) {
  result = list(model = paste0("Multi-seasonal hazard function fit with ", object$n.seasons, 
                               " seasons with periodicity ", object$period, ".\n\n"))
  result$estimates = object$estimates
  if (date) {
    result$estimates = lapply(result$estimates, function(obj) {
      if (length(obj) == 5) {
        #i.e., if it isn't the mean hazard list element
        obj[1, 3:5] = format(as.Date('2000-12-31') + as.integer(obj[1,3:5]), format = '%m-%d')
        #doesn't matter what year we use (2000 here) because we're displaying dates as MM-DD
      }
      obj
    })
  }
  result$analysis = paste0("Log-likelihood: ", round(object$logLik, 4), "; AIC: ", round(object$AIC, 4))
  result
}

#' @export
print.cmfit = function(x, ...) {
  cat(paste0("Multi-seasonal hazard function fit with ", x$n.seasons, " seasons with periodicity ", x$period, ".\n\n"))
  print(x$estimates)
  cat(paste0("Log-likelihood: ", round(x$logLik, 3), "; AIC: ", round(x$AIC, 3), "\n"))
}
