#'Obtain log-likelihood value from a data set given a set of parameter values
#'
#'@param T times of death or censoring as Surv objects
#'@param gamma average annual hazard
#'@param mus k-vector of peaks
#'@param rhos k-vector of rhos (concentration parameters)
#'@param omegas k-vector of weights
#'
#'@return the maximum likelihood value for this set of data
#'
#'@example examples/loglike_example.R
#'@export

loglike <- function(T, gamma, mus, rhos, omegas) {
  TT = as.matrix(T)
  D = TT[,2]
  TT = TT[,1]
  hazard = gamma * mwc(TT, mus = mus, rhos = rhos, omegas = omegas, tau = 1)
  cumhazard = gamma * imwc(TT, mus = mus, rhos = rhos, omegas = omegas, tau = 1)
  cum.prob.survival <-  exp(-cumhazard)
  F <- 1 - cum.prob.survival
  f <- hazard * cum.prob.survival
  sum(D * log(f) + (1-D) * log(1-F))
}

#'Log-likelihood function that is useable for the optim command
#'
#'@param T times of death or censoring as Surv objects
#'@param pars named vector including "A", "peak", "weight" and "rho" names
#'@param dt interval for plots as well as precision of random samples
#'
#'@return likelihood value given named vector of parameters as well as set of observations
#'
#'@export

loglike_optim<- function(pars, T) {
  
  -loglike(T = T, 
           gamma = pars["meanhazard"], 
           mus = pars[grepl("peak", names(pars))],
           rhos = findRho(pars[grepl("duration", names(pars))]),
           omegas = pars[grepl("weight", names(pars))])
}
