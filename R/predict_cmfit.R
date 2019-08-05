#' Prediction method for cyclomort fits
#' 
#' Obtain predictions and - optionally - confidence intervals for the hazard function from a fitted periodic hazard functon. Confidence intervals are produced by assuming that the parameters follow a multivariate normal distribution 
#' 
#' @param x a cmfit object
#' @param t times for prediction
#' @param type either "hazard" or "timetoevent" - dictates what function is called
#' @param CI whether or not to compute 95\% confidence intervals
#' @param nreps number of samples drawn to generate confidence intervals.  The default 10^4 is more than enough, but is a little sluggish. 10^3 gives slightly rougher intervals, but is very fast
#' 
#' @example examples/predict_cmfit_example.R
#' @export


predict.cmfit <- function(x, t = seq(0, x$period, length = 1e2), 
                          type = "hazard", CI = FALSE, CI.level = 0.05, nreps = 1e4){
  
  if (! type %in% c("hazard", "timetoevent")) stop("Type must be 'hazard' or 'timetoevent'")
  if (CI.level <= 0 | CI.level >= 0.5) stop("Invalid confidence level")
  
  needToFixVectorFlag = (length(t) == 1)
  if (needToFixVectorFlag) t = c(t, t)
  if (type == "timetoevent") {
    if (x$k > 1) {
      Mu <- x$optim$par
      Sigma <- solve(x$optim$hessian)
      
      lrhos = Mu[grepl("lrho", names(Mu))]
      mus = Mu[grepl("mu", names(Mu))] * x$period
      gammas = Mu[grepl("gamma", names(Mu))] / x$period
      
      cumsurvfun = function(t1, t0, mus, rhos, gammas, tau) {
        exp(-(imwc(t1, mus, rhos, gammas, tau) - imwc(t0, mus, rhos, gammas, tau))) - 0.5
      }
      timetodeathfun = function(time, mus, rhos, gammas, tau) {
        uniroot(cumsurvfun, t0 = time, mus = mus, rhos = rhos, gammas = gammas, tau = tau, interval = unlist(c(time, time + 5/x$estimates$meanhazard[1])))$root - time
      }
      
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
        CIs <- apply(sample.fits, 2, quantile, c(CI.level / 2, 1 - CI.level / 2))
      } else {
        CIs <- rbind(rep(log(2) / x$estimates$meanhazard[3], 1e2), rep(log(2) / x$estimates$meanhazard[2], 1e2))
      }
    } else CIs <- NULL
    
    if (CI) CI[1, CI[1,] < 0] = 0
    
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
      CIs <- apply(sample.fits, 2, quantile, c(CI.level / 2, 1 - CI.level / 2))
    } else {
      CIs <- rbind(rep(x$estimates$meanhazard[2], 1e2), rep(x$estimates$meanhazard[3], 1e2))
    }
  } else CIs <- NULL
  
  if (CI) {
    if(needToFixVectorFlag & CI[1] < 0) {
      CI[1] = 0
    } else if (!needToFixVectorFlag) {
      CI[1, CI[1,] < 0] = 0
    }
  }
  
  if (needToFixVectorFlag) return(list(t = t[1], fit = hazard.hat[1], CI = CIs[,1]))
  return(list(t = t, fit = hazard.hat, CI = CIs, type = type))
}

logit <- function(p){ log(p/(1-p)) }

expit <- function(p){ exp(p)/(1+exp(p)) }