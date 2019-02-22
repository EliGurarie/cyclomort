#' Get 95% confidence intervals for each parameter that is estimated by the MLE
#' 
#' @param fit returned value from call to "optim" containing parameter estimates and Hessian matrix
#' 
#' @return confidence intervals for each parameter in the estimate
#' 
#' @example examples/cyclomortFit_example.R
#' @export

getCIs <- function(fit){
  p <- fit$par
  CIs <- sqrt(diag(solve(fit$hessian)))
  cbind(estimate = p, CI.low = p - 2*CIs, CI.high = p + 2*CIs)
}