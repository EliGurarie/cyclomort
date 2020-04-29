#' Prediction method for cyclomort fits
#' 
#' Obtain predictions and confidence intervals for the hazard function or the 
#' time to event from a fitted cyclomort object.
#' 
#' @details Confidence intervals are produced by sampling from the multivariate 
#' normal distribution of the MLE parameter estimates accounting for the 
#' covariance in the estimates by using the Hessian of the MLE.  
#' 
#' @param object a cmfit object
#' @param ... (not implemented)
#' @param t times for prediction.  By default, covers 100 observations over a 
#' single period.
#' @param type either \code{hazard} or \code{timetoevent} - dictates what 
#' exactly will be predicted
#' @param CI a boolean dictating whether or not to compute confidence intervals
#' @param CI.level confidence level (default 0.95) for CIs (if CI is TRUE)
#' @param nreps number of samples drawn to generate confidence intervals.  The 
#' default 10^3 is generally sufficient, and very fast for the hazard function, 
#' but possibly prohibitively slow for the time-to-event functionality.  
#' 
#' @return a list of vectors containing predictions for each value in \code{t}, 
#' as well as (optional) confidence intervals.
#' 
#' @example examples/predict_cmfit_example.R
#' @export

predict.cmfit <- function(object, ..., t = seq(0, object$period, length = 5e2),
                          type = "hazard", CI = FALSE, CI.level = 0.95, 
                          nreps = 1e3) {

  # some functions for time to event modeling
  ## square of cumulative survival function at interval t0 to t1 - 1/2
      cumsurvsqd <- function(t1, t0, mus, rhos, gammas, tau){
        (exp(-(imwc(t1, mus, rhos, gammas, tau) - 
                 imwc(t0, mus, rhos, gammas, tau))) - 0.5)^2
      }
  ## computes median time to event starting at time "time" with given parameters
    timetoeventfun <- function(time, mus, rhos, gammas, tau, meanhazard) {
      optimize(cumsurvsqd, t0 = time, mus = mus, rhos = rhos, 
               gammas = gammas, tau = tau, 
               interval = c(time, time + 3/meanhazard))$minimum - time
    }
  ## vectorized version of timetoevent function
    ttefun.vec <- Vectorize(timetoeventfun, vectorize.args = c("time"))
  
  if (! type %in% c("hazard", "timetoevent")) 
    stop("Type must be one of 'hazard' or 'timetoevent'")
  if (CI.level < 0 | CI.level > 1) stop("Invalid confidence level")
  if(!CI) CI.level <- NULL
        
  needToFixVectorFlag = length(t) == 1
  if (needToFixVectorFlag) t = c(t, t)
  
  # number of peaks k >  1
  if (object$k > 1) {
    Mu <- object$optim$par
    Sigma <- solve(object$optim$hessian)
    tau <- object$period
    
    rhos <- expit(Mu[grepl("lrho", names(Mu))])
    mus <- Mu[grepl("mu", names(Mu))] * tau
    gammas <- Mu[grepl("gamma", names(Mu))] / tau
    mh <- object$estimates$meanhazard[1,1]
    
    if(type == "hazard")
      fit <- mwc(t = t, mus = mus, rhos = rhos, gammas = gammas, tau = tau)
    
    if(type == "timetoevent")
      fit <- ttefun.vec(t, mus = mus, rhos = rhos, gammas = gammas, 
                                 tau = tau, meanhazard = mh)
    
    if(CI){
      pars.sample <- mvtnorm::rmvnorm(nreps, Mu, Sigma)
      
      mu.index <- grep("mu", colnames(pars.sample))
      lrho.index <- grep("lrho", colnames(pars.sample))
      gamma.index <- grep("gamma", colnames(pars.sample))
      
      if(type == "hazard"){
        message(paste0("Estimating ", CI.level*100, 
                       "% confidence intervals for the hazard function drawing ", 
                       nreps, " samples from the parameter estimates.\n"))
        sample.fits <- aaply(pars.sample, 1, function(p){
          mwc(t, mus = p[mu.index] * tau, 
              rhos = expit(p[lrho.index]),
              gammas = p[gamma.index] / tau,
              tau = tau)
        })
      }
        if(type == "timetoevent"){
          
          message(paste0("Estimating ", CI.level*100, 
                         "% confidence intervals for the expected *time to event* using ", 
                         nreps, " samples from the parameter estimates."))
          if(nreps >= 1e3)
            warning(paste0("Using 1000 or more samples to obtain confidence intervals for the expected *time to event* process might be prohibitively slow.  Consider lowering the nreps to, e.g, 100."))
          
          sample.fits <- aaply(pars.sample, 1, function(p){
            ttefun.vec(t,  mus = p[mu.index] * tau, 
                       rhos = expit(p[lrho.index]),
                       gammas = p[gamma.index] / tau, 
                       tau = tau,
                       meanhazard = mh)
          })
        }
        CIs <- apply(sample.fits, 2, quantile, 
                     c((1 - CI.level)/2, (1 + CI.level)/2))
    } else CIs <- NULL
  }

  # number of peaks = 1
  if (object$k == 1){
    if(type == "hazard"){
      fit <- rep(object$estimates$meanhazard[1], length(t))
      if(CI) {
        CIs <- rbind(rep(object$estimates$meanhazard[2], length(t)), 
                     rep(object$estimates$meanhazard[3], length(t)))
      } else CIs <- NULL}
    
    if (type == "timetoevent"){
      timetoevent.hat <- rep(log(2) / object$estimates$meanhazard[1,1], 
                             length(t))
      if(CI) {
        CIs <- rbind(rep(log(2) / object$estimates$meanhazard[3], length(t)), 
                     rep(log(2) / object$estimates$meanhazard[2], length(t)))
      } else CIs <- NULL}
  }
  
  if (needToFixVectorFlag) 
    result <- list(t = t[1], fit = fit, CI = CIs[,1], CI.level = CI.level, 
                   type = type, nreps = nreps) else
      result <- list(t = t, fit = fit, 
                     CI = CIs, CI.level = CI.level, 
                     type = type, nreps = nreps)
  
  if (any(result$fit < 0) | any(result$CI < 0)) 
    warning("Some sampled values were less than zero. Manually correcting to provide sensible results.")
  result$fit[result$fit < 0] = 0
  result$CI[result$CI < 0] = 0
  
  return(result)
}

logit <- function(p){ log(p/(1-p)) }
expit <- function(p){ exp(p)/(1+exp(p)) }

