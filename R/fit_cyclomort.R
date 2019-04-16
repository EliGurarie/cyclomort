#'Get maximum likelihood estimates for periodic mortality data. Uses the optim
#'function along with a named vector of initial guesses and returns the parameter
#'estimates.
#'
#'@param T a cycloSurv object recording start and end times as well as status (dead/censored) and the length of one full period
#'@param p0 set of initial guesses; a named vector or list with values for "peak" and "duration". Leaving some or all of these parameters as NULL will trigger the automatic selection of an initial guess.
#'@param n.seasons expected number of seasons if p0 is not entirely filled out
#'@param method method for optim call
#'@param hessian parameter for optim call
#'
#'@return parameter estimates for weights, rhos, peaks and A
#'
#'@example examples/cyclomortFit_example.R
#'@export

fit_cyclomort = function(T, inits = NULL, n.seasons = 2, method = "L-BFGS-B", hessian = TRUE) {
  
  # normalize to period 1
  
    period = attributes(T)$period
    if (is.null(period)) period = 1
    # times
    T[,1:2] <- T[,1:2]/period
    
    # durations & peaks
    if(!is.null(inits)) inits <- inits/period
  
  T[T[,2] <= T[,1], 2] = T[T[,2] <= T[,1], 1] + 1e-6
  #sometimes simPeriodicMorts puts out slightly odd numbers that can easily be corrected
  
  null_fits = flexsurvreg(T ~ 1, dist = "exp", inits = c(rate = 1))
  
  if (is.null(inits)) {
    ##fitting procedure for initial guesses
    p0 = generateInitialParameterEstimate(T, n.seasons, null_fits)
  } else {
    ##the user has submitted default initial guesses for parameters
    durations = inits[grepl("duration", names(inits))]
    peaks = inits[grepl("peak", names(inits))]
    n.seasons = length(inits) / 2
    gammas = rep(null_fits$res[1,'est'] / n.seasons, n.seasons)
    mus = peaks
    names(mus) <- NULL
    rhos = findRho(durations)
    names(rhos) <- NULL
    p0 = unlist(list(gamma = gammas, rho = rhos, mu = mus))
  }
  
  cm = list(n.seasons = n.seasons)
  if (n.seasons == 0) {
    ##null model
    meanhazard = null_fits$res[1,1:4] / period
    names(meanhazard) <- c("estimate", "CI.low", "CI.high", "se")
    cm$estimates = list(meanhazard = meanhazard) # mortality rate a.k.a. average hazard
    cm$logLik = logLik(null_fits)
    cm$AIC = AIC(null_fits)
    cm$BIC = BIC(null_fits)
  } else {
    
    lower <- p0 * 0 + 1e-6
    upper <- ceiling(p0) - 1e-6
    upper[grepl("gamma", names(upper))] <- Inf
    
    if(method %in% c("L-BFGS-B", "Brent")){
      fits = optim(p0, loglike_optim, 
                   T = T, hessian = hessian, 
                   method = method,
                   lower = lower, upper = upper)
    } else {
      fits = optim(p0, loglike_optim, 
                   T = T, hessian = hessian, 
                   method = method)
    }
    
    CIs = getCIs(fit = fits)
    
    gammas.hat <- fits$par[grepl("gamma", names(fits$par))]
    rhos.hat <- fits$par[grepl("rho", names(fits$par))]
    mus.hat <- fits$par[grepl("mu", names(fits$par))]
    
    ses <- sqrt(diag(solve(fits$hessian)))
    gammas.se <- ses[grepl("gamma", names(ses))]
    rhos.se <- ses[grepl("rho", names(ses))]
    mus.se <- ses[grepl("mu", names(ses))]
    
    ## Mean Hazard
    meanhazard.hat <- sum(gammas.hat) / period
    meanhazard.se <- (1/n.seasons) * sqrt(sum(gammas.se^2)) / period
    meanhazard.CI <- meanhazard.hat + c(-2,2)*meanhazard.se
    
    ## Weights
    weights.hat <- gammas.hat / (meanhazard.hat * period)
    weights.se <- gammas.se / (meanhazard.hat * period)
    weights.CI <- weights.hat + (weights.se) %*% t(c(-2,2))
    
    ## Peaks 
    peaks.hat <- mus.hat * period
    peaks.se <- mus.se * period
    peaks.CI <- peaks.hat + peaks.se %*% t(c(-2,2))
    
    ## Durations
    durations.hat <- findDelta(rhos.hat) * period
    
    rhos.lower <- pmax(rhos.hat - 2*rhos.se, 0)
    rhos.upper <- pmin(rhos.hat + 2*rhos.se, 1)
    
    durations.low <- findDelta(rhos.upper) * period
    durations.high <- findDelta(rhos.lower) * period
    durations.CI <- cbind(durations.low, durations.high)
    durations.se <- 1/4 * (durations.high - durations.low)
    
    pointestimates <- data.frame(estimate = c(weights.hat, peaks.hat, durations.hat), 
                                 CI = rbind(weights.CI, peaks.CI, durations.CI), 
                                 se = c(weights.se, peaks.se, durations.se))
    names(pointestimates)[2:3] <- c("CI.low", "CI.high")
    rn <- row.names(pointestimates)
    pointestimates$season <- substr(rn, nchar(rn), nchar(rn)) %>% as.numeric
    parnames <- substr(rn, 1, nchar(rn)-1) 
    parnames[parnames == "gamma"] = "weight"
    parnames[parnames == "rho"] = "duration"
    parnames[parnames == "mu"] = "peak"
    pointestimates$parameter <- parnames
    
    pointestimates <- pointestimates[,c("parameter","season","estimate", "CI.low","CI.high","se")]
    ordered.seasons <- (subset(pointestimates, parameter == "peak") %>% arrange(estimate))$season
    pointestimates$season <- ordered.seasons[pointestimates$season]
    
    seasonalfits <- dlply(pointestimates, "season")
    names(seasonalfits) <- paste("season", 1:n.seasons)
    
    cm$estimates <- list(meanhazard = data.frame(estimate = meanhazard.hat, 
                                                 CI.low = meanhazard.CI[1],
                                                 CI.high = meanhazard.CI[2],
                                                 se = meanhazard.se)) %>% append(seasonalfits)
    
    cm$optim = fits
    cm$logLik = -fits$value
    cm$AIC = 2 * fits$value + 2 * n.seasons*3
    cm$BIC = 2 * fits$value + log(length(T)) * n.seasons * 3
    cm$k = 3*n.seasons
  }
  cm$period = period
  cm$data = T
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
  
  deaths = T[which(T[,3] == 1),2]
  normdata = deaths - floor(deaths) # assuming period == 1 - will give times of death within the period
  if (n.seasons == 1) {
    mu = mean(normdata)
    sigma = sd(normdata) # this is the MLE for standard deviation of Gaussian distribution, right?
    delta = qnorm(0.75, 0, sigma) * 2
    if (delta >= 0.5) delta = 0.5 - 1e-6 #hopefully this doesn't happen - it shouldn't too much based on my tests!
    result = c(result, meanhazardvalue, mu, delta)
  } else {
    normFits = suppressMessages(normalmixEM(x = normdata, k = n.seasons))
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
  se <- sqrt(diag(solve(fit$hessian)))
  cbind(estimate = p, CI.low = p - 2*se, CI.high = p + 2*se, se = se)
}
