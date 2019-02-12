#'Log-likelihood function that is useable for the optim command
#'
#'@export

loglike_optim<- function(pars, T, dt, period) {
  
  -loglike(T = T, dt = dt, period = period, 
           A = pars["A"], 
           p = pars[grepl("peak", names(pars))],
           r = pars[grepl("rho", names(pars))],
           w = pars[grepl("weight", names(pars))])
  
}