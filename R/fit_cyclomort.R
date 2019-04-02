#'Get maximum likelihood estimates for periodic mortality data. Uses the optim
#'function along with a named vector of initial guesses and returns the parameter
#'estimates.
#'
#'@param T set of Surv objects representing time of death or censorship
#'@param p0 set of initial guesses; a named vector or list with values for "peak", "duration", and "weight". Leaving some or all of these parameters as NULL will trigger the automatic selection of an initial guess.
#'@param n.seasons expected number of seasons if p0 is not entirely filled out
#'
#'@return parameter estimates for weights, rhos, peaks and A
#'
#'@example examples/cyclomortFit_example.R
#'@export

fit_cyclomort = function(T, p0 = NULL, n.seasons = 2) {
  
  if (is.null(p0)) {
    ##fitting procedure for initial guesses
  }
  
  cm = list()
  period = attributes(T)$period
  if (n.seasons == 0) {
    ##null model
    require(flexsurv)
    fits = flexsurvreg(T ~ 1, dist = "exp")
    cm$meanhazard = fits[[18]][1:3] # mortality rate a.k.a. average hazard
    cm$logLik = logLik(fits)
    cm$AIC = AIC(fits)
  } else {
    fits = optim(p0, loglike_optim, T = T, hessian = TRUE)
    CIs = getCIs(fit = fits)
    fitNames = names(fits$par)
    if (is.null(period)) period = 1
    extra = 1
    for (i in 1:length(fitNames)) {
      if (grepl("peak", fitNames[i])) {
        cm[[i]] = CIs[i, ] * period
      } else if (grepl("rho", fitNames[i])) {
        cm[[i]] = CIs[i, ]
        cm[[length(fitNames) + extra]] = (Vectorize(getSeasonLength)(CIs[i, ]) * period)[c(1,3,2)]
        names(cm)[length(fitNames) + extra] = paste0("season", substr(fitNames[i], nchar(fitNames[i]), nchar(fitNames[i])))
        extra = extra + 1
      } else {
        cm[[i]] = CIs[i, ]
      }
      names(cm)[i] = fitNames[i]
    }
    cm$hessian = fits$hessian
    cm$logLik = fits$value
    cm$AIC = -2 * fits$value + 2 * length(fitNames)
    cm$counts = fits$counts
    cm$convergence = fits$convergence
  }
  cm$period = period
  cm$data = T
  cm$dt = dt
  class(cm) = "cmfit"
  cm
}

#' Produce initial parameter estimates based on mortality data
#' 
#' Uses a basic flexsurvreg exponential mortality model to find the average hazard value, and fits a mixed normal distribution model to estimate the peaks, season durations, and weight distributions for the model. These estimates are not meant to be fully accurate but instead are meant to be good initial guesses for the fit_cyclomort function.
#' 
#' @param T set of Surv objects representing time of death or censorship
#' @param n.seasons expected number of seasons within a period
#' 
#' @return a named vector listing intial guesses for parameter values, to be used in the fitting process
#' 
#' @export
generateInitialParameterEstimate = function(T, n.seasons) {
  require(flexsurv)
  require(mixtools)
  
  result = c() # blank vector to be filled up with parameters
  
  null_fits = flexsurvreg(T ~ 1, dist = "exp")
  meanhazardvalue = fits[[18]][1] # mortality rate a.k.a. average hazard for entire distribution
  
  normFits = normalMixEM(x = T, k = n.seasons)
  
  result = c(result, normFits$lambda * meanhazardvalue)
  ##to do later!!!
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
  value = cbind(estimate = p, CI.low = p - 2*CIs, CI.high = p + 2*CIs)
  value[is.na(value[,2]),2] = 0
  value[is.na(value[,3]),3] = 0.9999
  value
}