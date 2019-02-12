#'Log-likelihood function that is useable for the optim command
#'
#'@param T times of death or censoring as Surv objects
#'@param pars named vector including "A", "peak", "weight" and "rho" names
#'@param period period of one mortality cycle
#'@param dt interval for plots as well as precision of random samples
#'
#'@value likelihood value given named vector of parameters as well as set of observations
#'
#'@export

loglike_optim<- function(pars, T, dt, period) {
  
  -loglike(T = T, dt = dt, period = period, 
           A = pars["A"], 
           p = pars[grepl("peak", names(pars))],
           r = pars[grepl("rho", names(pars))],
           w = pars[grepl("weight", names(pars))])
  
}