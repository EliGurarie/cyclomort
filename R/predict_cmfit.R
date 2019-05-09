#' Prediction method for cyclomort fits
#' 
#' Obtain predictions and - optionally - confidence intervals for the hazard function from a fitted periodic hazard functon.  
#' 
#' @param x a cmfit object
#' @param t times for prediction
#' @param type either "hazard" or "timetoevent" - dictates what function is called
#' @param CI whether or not to compute 95\% confidence intervals
#' @param nreps number of samples drawn to generate confidence intervals.  The default 10^4 is more than enough, but is a little sluggish. 10^3 gives slightly rougher intervals, but is very fast
#' 
#' @examples
#' T <- simPeriodicMorts(300, period = 1, peaks = c(0.3, 0.8), 
#'                       durations = c(0.15, 0.20), weights = c(3, 2)/5, 
#'                      meanhazard = 1, plotme = TRUE, max.periods = 6)
#' T.fit <- fit_cyclomort(T, n.seasons = 2)
#' predict(T.fit, t = c(0.5, 0.5), CI = TRUE)
#' 
#' # The plotting method uses this function.
#' plot(T.fit, CI = TRUE, nreps = 1e3, breaks = 40)
#' @export


predict.cmfit <- function(x, t = seq(0, x$period, length = 1e2), 
                          type = "hazard", CI = FALSE, nreps = 1e4){
  
  if (! type %in% c("hazard", "timetodeath")) stop("Type must be 'hazard' or 'timetodeath'")
  
  needToFixVectorFlag = (length(t) == 1)
  if (needToFixVectorFlag) t = c(t, t)
  if (type == "timetodeath") {
    timetodeathfun = function(time, mus, rhos, gammas, tau) {
      cumsurvfun = function(t) {
        exp(-(imwc(t, mus, rhos, gammas, tau) - imwc(time, mus, rhos, gammas, tau))) - 0.5
      }
      uniroot(cumsurvfun, interval = unlist(c(time, time + 5/x$estimates$meanhazard[1])))$root - time
    }
    if (x$k > 1) {
      Mu <- x$optim$par
      Sigma <- solve(x$optim$hessian)
      
      lrhos = Mu[grepl("lrho", names(Mu))]
      mus = Mu[grepl("mu", names(Mu))] * x$period
      gammas = Mu[grepl("gamma", names(Mu))] / x$period
      
      timetodeath.hat <- Vectorize(timetodeathfun, vectorize.args = c("time"))(t, mus, expit(lrhos), gammas, x$period) / x$period
    } else { 
      timetodeath.hat <- rep(log(2) / x$estimates$meanhazard[1], 1e2)
    }
    
    if(CI){
      if (x$k > 1) {
        pars.sample <- mvtnorm::rmvnorm(nreps, Mu, Sigma)
        parnames <- colnames(pars.sample)
        sample.fits <- aaply(pars.sample, 1, function(p){
          Vectorize(timetodeathfun, vectorize.args = c("time"))(t,
              mus = p[grep("mu", parnames)] * x$period, 
              rhos = expit(p[grep("rho", parnames)]),
              gammas = p[grep("gamma", parnames)] / x$period,
              tau = 1) / x$period
        })
        CIs <- apply(sample.fits, 2, quantile, c(0.025, 0.975))
      } else {
        CIs <- rbind(rep(log(2) / x$estimates$meanhazard[3], 1e2), rep(log(2) / x$estimates$meanhazard[2], 1e2))
      }
    } else CIs <- NULL
    
    if (needToFixVectorFlag) return(list(t = t[1], fit = timetodeath.hat[1], CI = CIs[,1]))
    return(list(t = t, fit = timetodeath.hat, CI = CIs, type = type))
  }
  
  if (x$k > 1) {
    Mu <- x$optim$par
    Sigma <- solve(x$optim$hessian)
    
    lrhos = Mu[grepl("lrho", names(Mu))]
    mus = Mu[grepl("mu", names(Mu))]
    gammas = Mu[grepl("gamma", names(Mu))]
    hazard.hat <- mwc(t = t / x$period, mus = mus, rhos = expit(lrhos), gammas = gammas, tau = 1)
  } else { 
    hazard.hat <- rep(x$estimates$meanhazard[1], 1e2)
  }
  
  if(CI){
    if (x$k > 1) {
      pars.sample <- mvtnorm::rmvnorm(nreps, Mu, Sigma)
      parnames <- colnames(pars.sample)
      sample.fits <- aaply(pars.sample, 1, function(p){
        mwc(t / x$period, mus = p[grep("mu", parnames)], 
            rhos = expit(p[grep("rho", parnames)]),
            gammas = p[grep("gamma", parnames)],
            tau = 1)
      })
      CIs <- apply(sample.fits, 2, quantile, c(0.025, 0.975))
    } else {
      CIs <- rbind(rep(x$estimates$meanhazard[2], 1e2), rep(x$estimates$meanhazard[3], 1e2))
    }
  } else CIs <- NULL
  
  if (needToFixVectorFlag) return(list(t = t[1], fit = hazard.hat[1], CI = CIs[,1]))
  return(list(t = t, fit = hazard.hat, CI = CIs, type = type))
}
