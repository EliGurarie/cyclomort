#'Obtain log-likelihood value from a data set given a set of parameter values
#'
#'@param x a cycloSurv object
#'@param gammas k-vector of average hazard values for each component
#'@param mus k-vector of peaks
#'@param rhos k-vector of concentration parameters
#'
#'@return the maximum likelihood value for this set of data
#'
#'@export

loglike <- function(x, gammas, mus, rhos) {
  T_censoring = x[,3]
  T_end = x[,2]
  T_start = x[,1]
  T_diff = T_end - T_start
  hazard = mwc(T_end, mus = mus, rhos = rhos, gammas = gammas, tau = 1)
  cumhazard = imwc(T_end, mus = mus, rhos = rhos, gammas = gammas, tau = 1) - 
    imwc(T_start, mus = mus, rhos = rhos, gammas = gammas, tau = 1)
  cum.prob.survival <-  exp(-cumhazard)
  F <- 1 - cum.prob.survival
  f <- hazard * cum.prob.survival
  sum(T_censoring * log(f) + (1-T_censoring) * log(1-F))
}

#' Log-likelihood function 
#'
#' Internal function used for computing the log-likelihood of a parameterized 
#' model within \code{\link{fit_cyclomort}}.  
#'
#' @param x times of death or censoring as Surv objects
#' @param pars named vector including "gamma", "mu", and "rho" 
#' parameters for the appropriate number of seasons
#'
#'@return likelihood value given named vector of parameters as well as set of 
#'observations
#'@seealso \code{\link{fit_cyclomort}}
#'
#'@export

loglike_optim<- function(pars, x) {
  rhos <- expit(pars[grepl("lrho", names(pars))])
  -loglike(x = x, 
           gammas = pars[grepl("gamma", names(pars))], 
           mus = pars[grepl("mu", names(pars))],
           rhos = rhos)
}
logit <- function(p){ log(p/(1-p)) }
expit <- function(p){ exp(p)/(1+exp(p)) }
