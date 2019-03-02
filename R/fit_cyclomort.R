#'Get maximum likelihood estimates for periodic mortality data. Uses the optim
#'function along with a named vector of initial guesses and returns the parameter
#'estimates.
#'
#'@param T set of Surv objects representing time of death or censorship
#'@param p0 set of initial guesses; a named vector with "A", "peak", "rho" and "weight" names
#'@param period period of one mortality cycle
#'@param dt interval for plots as well as precision of random samples
#'
#'@return parameter estimates for weights, rhos, peaks and A
#'
#'@example examples/cyclomortFit_example.R
#'@export

fit_cyclomort = function(T, p0, dt) {
  fits = optim(p0, loglike_optim, T = T, dt = dt, hessian = TRUE)
  CIs = getCIs(fits = fits)
  cm = list()
  fitNames = names(fits$par)
  period = attributes(T)$period
  for (i in 1:length(fitNames)) {
    if (grepl("peak", fitNames[i])) {
      cm[[i]] = CIs[i] * period
    } else if (grepl("rho", fitNames[i])) {
      cm[[i]] = getSeasonLength(CIs[i]) * period
    } else {
      cm[[i]] = CIs[i]
    }
    names(cm)[i] = fitNames[i]
  }
  cm$period = period
  cm$hessian = fits$hessian
  cm$logLik = fits$value
  cm$AIC = -2 * fits$value + 2 * length(fitNames)
  cm$counts = fits$counts
  cm$convergence = fits$convergence
  class(cm) = "cmfit"
  cm
}

#' Get 95% confidence intervals for each parameter that is estimated by the MLE
#' 
#' @param fit returned value from call to "optim" containing parameter estimates and Hessian matrix
#' 
#' @return confidence intervals for each parameter in the estimate
#' 
#' @export

getCIs <- function(fit){
  p <- fit$par
  CIs <- sqrt(diag(solve(fit$hessian)))
  cbind(estimate = p, CI.low = p - 2*CIs, CI.high = p + 2*CIs)
}

#' Gets the season length in periods based on the concentration parameter rho
#' 
#' @param rho concentration parameter for wrapped Cauchy distribution between 0 and 1
#' 
#' @return length of season assuming period length of 1
#' 
#' @examples
#' plot(Vectorize(getSeasonLength)(seq(0.05, 0.95, 0.05)), type = "l")
#' @export

getSeasonLength = function(rho) {
  (-2 * qwrpCauchy(0.25, 0, rho)) / (2 * pi)
}