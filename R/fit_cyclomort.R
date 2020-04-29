#' Estimate periodic hazard function. 
#'
#' This function takes time-to-event data formatted as a \code{cycloSurv} object 
#' and estimates an underlying hazard function for a given number of seasons. 
#'
#'@param x a cycloSurv object recording start and end times as well as status 
#'(dead/censored) and the length of one full period
#'@param inits set of initial guesses; a named vector or list with values for 
#'"peak" and "duration". Leaving some or all of these parameters as NULL will 
#'trigger the automatic selection of an initial guess.
#'@param n.seasons number of seasons to fit model to
#'@param method method for optim call
#'@param period expected periodicity of survival data. Can be passed in with 
#'cycloSurv input parameter
#'
#'@return a cmfit object containing parameter estimates for peaks, durations, 
#'and weights for each season
#'
#'@example examples/cyclomortFit_example.R
#'@export

fit_cyclomort = function(x, inits = NULL, n.seasons = 2, method = "L-BFGS-B", 
                         period = NULL) {
  
  # normalize to period 1
  if (is.null(period))  period = attributes(x)$period
  if (is.null(period))  
    stop("Data does not contain a period attribute. Must be entered manually.\n")
  
  # times
  x[,1:2] <- x[,1:2]/period
  x[x[,2] <= x[,1], 2] = x[x[,2] <= x[,1], 1] + 1e-6
  
    null_fits = flexsurvreg(x ~ 1, dist = "exp")
  meanhazard = null_fits$res[1,1]
  
  p0 = guess_initial_parameters(x, n.seasons, null_fits)
  
  inits.vector <- unlist(inits)
  
  if(any(grepl("duration", names(inits.vector)))){
    durations = inits.vector[grepl("duration", names(inits.vector))] / period
    p0[grepl("lrho", names(p0))] <- logit(findRho(durations))
  }
  
  if(any(grepl("peak", names(inits.vector)))){
    mus = inits.vector[grepl("peak", names(inits.vector))]/period
    p0[grepl("mu", names(p0))] <- mus
  }
  
  cm = list(n.seasons = n.seasons)
  if (n.seasons == 0) {
    ##null model
    meanhazard = null_fits$res[1,1:4]/period
    names(meanhazard) <- c("estimate", "CI.low", "CI.high", "se")
    cm$estimates = list(meanhazard = meanhazard) 
    cm$logLik = logLik(null_fits)
    cm$AIC = null_fits$AIC
    # cm$BIC = BIC(null_fits)
    cm$k = 1
  } else {
    
    lower <- p0 * 0 + 1e-6
    lower[grepl("lrho", names(lower))] <- -Inf
    lower[grepl("gamma", names(lower))] <- meanhazard[1]/10
    
    upper <- ceiling(p0) - 1e-6
    upper[grepl("gamma", names(upper))] <- Inf
    upper[grepl("lrho", names(upper))] <- Inf
    
    if(method %in% "L-BFGS-B"){
      fits = optim(p0, loglike_optim,  
                   x = x, 
                   hessian = TRUE,
                   method = method,
                   lower = lower, upper = upper)
    } else {
      fits = optim(p0, loglike_optim, 
                   x = x, hessian = TRUE, 
                   method = method)
    }
    
    gammas.hat <- fits$par[grepl("gamma", names(fits$par))]
    lrhos.hat <- fits$par[grepl("lrho", names(fits$par))]
    mus.hat <- fits$par[grepl("mu", names(fits$par))]
    
    ses <- sqrt(diag(solve(fits$hessian)))
    gammas.se <- ses[grepl("gamma", names(ses))]
    lrhos.se <- ses[grepl("lrho", names(ses))]
    mus.se <- ses[grepl("mu", names(ses))]
    
    ## Mean Hazard
    meanhazard.hat <- sum(gammas.hat) / period
    meanhazard.se <- (1/n.seasons) * sqrt(sum(gammas.se^2)) / period
    meanhazard.CI <- meanhazard.hat + c(-2,2)*meanhazard.se
    if (any(is.na(meanhazard.CI))) 
      warning("Could not produce confidence intervals for mean hazard parameter. Interpret results with skepticism.\n")
    
    ## Weights
    weights.hat <- gammas.hat / (meanhazard.hat * period)
    weights.se <- gammas.se / (meanhazard.hat * period)
    weights.CI <- weights.hat + (weights.se) %*% t(c(-2,2))
    if (any(is.na(meanhazard.CI)) | 
        (weights.CI[1] < 1e-6 | weights.CI[1] > (1 - 1e-6))) 
      warning("Could not produce accurate confidence intervals for weight parameter. Interpret results with skepticism.\n")
    if (any(!is.na(weights.CI) & weights.CI > 1)) warning('Upper confidence limit for weights manually coerced to 1')
    weights.CI[weights.CI > 1] = 1
    weights.CI[weights.CI < 0] = 0
    
    ## Peaks 
    peaks.hat <- mus.hat * period
    peaks.se <- mus.se * period
    peaks.CI <- peaks.hat + peaks.se %*% t(c(-2,2))
    if (any(is.na(meanhazard.CI))) 
      warning("Could not produce confidence intervals for peak parameter. Interpret results with skepticism.\n")
    
    ## Durations
    durations.hat <- findDelta(expit(lrhos.hat)) * period
    
    rhos.lower <- expit(lrhos.hat - 2*lrhos.se)
    rhos.upper <- expit(lrhos.hat + 2*lrhos.se)
    
    durations.low <- findDelta(rhos.upper) * period
    durations.high <- findDelta(rhos.lower) * period
    durations.CI <- cbind(durations.low, durations.high)
    if (any(is.na(meanhazard.CI)) | (durations.CI[1] < 1e-6 | 
                                     durations.CI[1] > period * (1 - 1e-6))) 
      warning("Could not produce confidence intervals for duration parameter. Interpret results with skepticism.\n")
    
    if (n.seasons > 1) {
      pointestimates <- data.frame(estimate = c(peaks.hat, durations.hat, 
                                                weights.hat), 
                                   CI = rbind(peaks.CI, durations.CI, 
                                              weights.CI))
    } else {
      pointestimates <- data.frame(estimate = c(peaks.hat, durations.hat), 
                                   CI = rbind(peaks.CI, durations.CI))
    }
    
    names(pointestimates)[2:3] <- c("CI.low", "CI.high")
    rn <- row.names(pointestimates)
    pointestimates$season <- substr(rn, nchar(rn), nchar(rn)) %>% as.numeric
    parnames <- substr(rn, 1, nchar(rn)-1) 
    parnames[parnames == "gamma"] = "weight"
    parnames[parnames == "lrho"] = "duration"
    parnames[parnames == "mu"] = "peak"
    pointestimates$parameter <- parnames
    
    pointestimates <- pointestimates[,c("parameter","season","estimate", 
                                        "CI.low","CI.high")]
    ordered.seasons <- (subset(pointestimates, parameter == "peak") %>% 
                          arrange(estimate))$season
    pointestimates$season <- ordered.seasons[pointestimates$season]
    
    seasonalfits <- dlply(pointestimates, "season")
    names(seasonalfits) <- paste("season", 1:n.seasons)
    
    cm$estimates <- list(meanhazard = data.frame(estimate = meanhazard.hat, 
                                                 CI.low = meanhazard.CI[1],
                                                 CI.high = meanhazard.CI[2],
                                                 se = meanhazard.se)) %>% 
      append(seasonalfits)
    
    cm$optim = fits
    cm$logLik = -fits$value
    cm$AIC = 2 * fits$value + 2 * n.seasons*3
    cm$BIC = 2 * fits$value + log(length(x)) * n.seasons * 3
    cm$k = 3*n.seasons
  }
  cm$period = period
  cm$data = x
  class(cm) = "cmfit"
  cm
}

#' Produce initial parameter estimates based on mortality data
#' 
#' Uses a basic flexsurvreg exponential mortality model to find the average 
#' hazard value, and fits a mixed normal distribution model to estimate the 
#' peaks, season durations, and weight distributions for the model. These 
#' estimates are not meant to be fully accurate but instead are meant to be 
#' good initial guesses for the fit_cyclomort function.
#' 
#' @param x \code{cycloSurv} object representing time of death or censorship
#' @param n expected number of mortality seasons within a period
#' @param null_fits original estimate for mortality rate assuming constant 
#' hazard function
#' 
#' @return a named vector of guesses for parameter values, used to initialize 
#' the fitting process
#' 
#' 
#' @export
guess_initial_parameters = function(x, n, null_fits) {
  if (n == 0) return (NULL)
  result = c() 
  
  meanhazardvalue = null_fits[[18]][1] 
  result = c(rep(meanhazardvalue/n, n), 
             seq(1/n/2, 1 - 1/n/2, length = n), 
             rep(0, n))
  
  names(result) = c(paste0("gamma", 1:n), 
                    paste0("mu", 1:n),
                    paste0("lrho", 1:n))
  result
}

logit <- function(p){ log(p/(1-p)) }
expit <- function(p){ exp(p)/(1+exp(p)) }
