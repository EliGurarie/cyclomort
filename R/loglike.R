#'Obtain log-likelihood value from a data set given a set of parameter values
#'
#'@param T times of death or censoring as Surv objects
#'@param gammas k-vector of average hazard values for each component
#'@param mus k-vector of peaks
#'@param rhos k-vector of rhos (concentration parameters)
#'
#'@return the maximum likelihood value for this set of data
#'
#'@example examples/loglike_example.R
#'@export

loglike <- function(T, gammas, mus, rhos) {
  TT = as.matrix(T)
  D = TT[,2]
  TT = TT[,1]
  hazard = mwc(TT, mus = mus, rhos = rhos, gammas = gammas, tau = 1)
  cumhazard = imwc(TT, mus = mus, rhos = rhos, gammas = gammas, tau = 1)
  cum.prob.survival <-  exp(-cumhazard)
  F <- 1 - cum.prob.survival
  f <- hazard * cum.prob.survival
  sum(D * log(f) + (1-D) * log(1-F))
}

#'Log-likelihood function that is useable for the optim command
#'
#'@param T times of death or censoring as Surv objects
#'@param pars named vector including "weight", "peak", and "duration" parameters for the appropriate number of seasons
#'
#'@return likelihood value given named vector of parameters as well as set of observations
#'
#'@export

loglike_optim<- function(pars, T) {

  -loglike(T = T, 
           gammas = pars[grepl("weight", names(pars))], 
           mus = pars[grepl("peak", names(pars))],
           rhos = findRho(pars[grepl("duration", names(pars))]))
}
