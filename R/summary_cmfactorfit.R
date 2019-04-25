#' Produce a table explaining the results of a cmfactorfit model
#' 
#' @param x a cmfactorfit object
#' 
#' @return a table comparing log-likelihood and AIC between null and multi-factor model, as well as a p-value from likelihood ratio test
#' 
#' @example examples/cyclomortModel_example.R
#' @export

summary.cmfactorfit = function(x) {
  ll = c(x$ll_null, x$ll_alt)
  aic = c(x$aic_null, x$aic_alt)
  p_lrt = c(x$p)
  data = cbind(logLik = ll, AIC = aic, LRT_p = p_lrt)
  rownames(data) = c("null model", "multi-factor model")
  data
}