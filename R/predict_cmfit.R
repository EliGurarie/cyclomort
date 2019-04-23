#' Prediction method for cyclomort fits
#' 
#' Obtain predictions and - optionally - confidence intervals for the hazard function from a fitted periodic hazard functon.  
#' 
#' @param x a cmfit object
#' @param t times for prediction
#' @param CI whether or not to compute 95% confidence intervals
#' @param nreps number of samples drawn to generate confidence intervals.  The default 10^4 is more than enough, but is a little sluggish. 10^3 gives slightly rougher intervals, but is very fast
#' 
#' @example
#' T <- simPeriodicMorts(300, period = 1, peaks = c(0.3, 0.8), 
#'                       durations = c(0.15, 0.20), weights = c(3, 2)/5, 
#'                      meanhazard = 1, plotme = TRUE, max.periods = 6)
#' T.fit <- fit_cyclomort(T, n.seasons = 2)
#' predict(T.fit, t = c(0.5, 0.5), CI = TRUE)
#' 
#' # The plotting method uses this function.
#' plot(T.fit, CI = TRUE, nreps = 1e3, breaks = 40)
#' @export


predict.cmfit <- function(x, t = seq(0, x$period, length = 1e2), CI = FALSE, nreps = 1e4){
  Mu <- x$optim$par
  Sigma <- solve(x$optim$hessian)
  
  lrhos = Mu[grepl("lrho", names(Mu))]
  mus = Mu[grepl("mu", names(Mu))]
  gammas = Mu[grepl("gamma", names(Mu))]
  
  hazard.hat <- mwc(t = t, mus = mus, rhos = expit(lrhos), gammas = gammas, tau = 1)
  
  if(CI){
    pars.sample <- mvtnorm::rmvnorm(nreps, Mu, Sigma)
    parnames <- colnames(pars.sample)
    sample.fits <- aaply(pars.sample, 1, function(p){
      mwc(t, mus = p[grep("mu", parnames)], 
          rhos = expit(p[grep("rho", parnames)]),
          gammas = p[grep("gamma", parnames)],
          tau = T.fit$period)
    })
    CIs <- apply(sample.fits, 2, quantile, c(0.025, 0.975))
  } else CIs <- NULL
  
  return(list(t = t, fit = hazard.hat, CI = CIs))
}
