#'Get maximum likelihood estimates for periodic mortality data. Uses the optim
#'function along with a named vector of initial guesses and returns the parameter
#'estimates.
#'
#'@param T set of Surv objects representing time of death or censorship
#'@param p0 set of initial guesses; a named vector or list with values for "peak" and "duration". Leaving some or all of these parameters as NULL will trigger the automatic selection of an initial guess.
#'@param n.seasons expected number of seasons if p0 is not entirely filled out
#'
#'@return parameter estimates for weights, rhos, peaks and A
#'
#'@example examples/cyclomortFit_example.R
#'@export

fit_cyclomort = function(T, p0 = NULL, n.seasons = 2) {
  
  if (min(T[,1]) <= 0) {
    T[T[,1] <= 0] = Surv(1e-6, 1)
  }
  null_fits = flexsurvreg(T ~ 1, dist = "exp", inits = c(rate = 1))
  period = attributes(T)$period
  if (is.null(period)) period = 1
  
  if (is.null(p0)) {
    ##fitting procedure for initial guesses
    p0 = generateInitialParameterEstimate(T, n.seasons, null_fits)
  } else {
    ##the user has submitted default initial guesses for parameters
    durations = pars[grepl("duration", names(pars))]
    peaks = pars[grepl("peak", names(pars))]
    n.seasons = length(p0) / 2
    gammas = null_fits[[18]][1] / n.seasons
    mus = peaks
    rhos = findRho(durations)
    p0 = unlist(list(gamma = gammas, rho = rhos, mu = mus))
  }
  
  cm = list()
  if (n.seasons == 0) {
    ##null model
    cm$meanhazard = null_fits[[18]][1:3] # mortality rate a.k.a. average hazard
    cm$logLik = logLik(null_fits)
    cm$AIC = AIC(null_fits)
    cm$rawpars = cm$meanhazard
  } else {
    fits = optim(p0, loglike_optim, T = T, hessian = TRUE)
    CIs = getCIs(fit = fits)
    fitNames = names(fits$par)
    meanhazardestimate = sum(fits$par[grepl("gamma", fitNames)])
    for (i in 1:length(fitNames)) {
      if (grepl("mu", fitNames[i])) {
        cm[[i]] = CIs[i, ] * period
        names(cm)[i] = paste0("peak", substr(fitNames[i], nchar(fitNames[i]), nchar(fitNames[i])))
      } else if (grepl("rho", fitNames[i])) {
        cm[[i]] = findDelta(CIs[i, ])
        names(cm)[i] = paste0("duration", substr(fitNames[i], nchar(fitNames[i]), nchar(fitNames[i])))
      } else { #if (grepl("gamma", fitNames[i]))
        cm[[i]] = CIs[i, ] / meanhazardestimate
        names(cm)[i] = paste0("weights", substr(fitNames[i], nchar(fitNames[i]), nchar(fitNames[i])))
      }
    }
    if (n.seasons != 1) {
      cm$meanhazard = colSums(CIs[grepl("gamma",fitNames),])
    } else {
      cm$meanhazard = CIs[grepl("gamma", fitNames),]
    }
    cm$rawpars = CIs
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
#' @param null_fits original estimate for mortality rate assuming constant hazard function
#' 
#' @return a named vector listing intial guesses for parameter values, to be used in the fitting process
#' 
#' @export
generateInitialParameterEstimate = function(T, n.seasons, null_fits) {
  if (n.seasons == 0) {
    return (NULL)
  }
  result = c() # blank vector to be filled up with parameters
  
  meanhazardvalue = null_fits[[18]][1] # mortality rate a.k.a. average hazard for entire distribution
  
  deaths = T[which(T[,2] == 1),1]
  normdata = deaths - floor(deaths) # assuming period == 1 - will give times of death within the period
  if (n.seasons == 1) {
    mu = mean(normdata)
    sigma = sd(normdata) # this is the MLE for standard deviation of Gaussian distribution, right?
    delta = qnorm(0.75, 0, sigma) * 2
    if (delta >= 0.5) delta = 0.5 - 1e-6 #hopefully this doesn't happen - it shouldn't too much based on my tests!
    result = c(result, meanhazardvalue, mu, delta)
  } else {
    normFits = normalmixEM(x = normdata, k = n.seasons)
    sigmas = normFits$sigma
    deltas = qnorm(0.75, 0, sigmas) * 2
    deltas[deltas >= 0.5] = 0.5 - 1e-6 #hopefully this doesn't happen - it shouldn't too much based on my tests!
    result = c(result, normFits$lambda * meanhazardvalue, normFits$mu, findRho(deltas))
  }
  seasonNumbers = 1:(n.seasons)
  names(result) = c(paste0("gamma", seasonNumbers), paste0("mu", seasonNumbers), paste0("rho", seasonNumbers))
  result
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