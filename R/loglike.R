#'Obtain log-likelihood value from a data set given a set of parameter values
#'
#'@param T a cycloSurv object
#'@param gammas k-vector of average hazard values for each component
#'@param mus k-vector of peaks
#'@param rhos k-vector of rhos (concentration parameters)
#'
#'@return the maximum likelihood value for this set of data
#'
#'@example examples/loglike_example.R
#'@export

loglike <- function(T, gammas, mus, rhos) {
  T_censoring = T[,3]
  T_end = T[,2]
  T_start = T[,1]
  T_diff = T_end - T_start
  hazard = mwc(T_end, mus = mus, rhos = rhos, gammas = gammas, tau = 1)
  cumhazard = imwc(T_end, mus = mus, rhos = rhos, gammas = gammas, tau = 1) - imwc(T_start, mus = mus, rhos = rhos, gammas = gammas, tau = 1)
  cum.prob.survival <-  exp(-cumhazard)
  F <- 1 - cum.prob.survival
  f <- hazard * cum.prob.survival
  sum(T_censoring * log(f) + (1-T_censoring) * log(1-F))
}

#'Log-likelihood function that is useable for the optim command
#'
#'@param T times of death or censoring as Surv objects
#'@param pars named vector including "gamma", "mu", and "rho" (because this function is entirely internal) parameters for the appropriate number of seasons
#'
#'@return likelihood value given named vector of parameters as well as set of observations
#'
#'@export

loglike_optim<- function(pars, T) {

  -loglike(T = T, 
           gammas = pars[grepl("gamma", names(pars))], 
           mus = pars[grepl("mu", names(pars))],
           rhos = pars[grepl("rho", names(pars))])
}
