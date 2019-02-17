#'Get maximum likelihood estimates for periodic mortality data. Uses the optim
#'function along with a named vector of initial guesses and returns the parameter
#'estimates.
#'
#'@param T set of Surv objects representing time of death or censorship
#'@param p0 set of initial guesses; a named vector with "A", "peak", "rho" and "weight" names
#'@param period period of one mortality cycle
#'@param dt interval for plots as well as precision of random samples
#'
#'@value parameter estimates for weights, rhos, peaks and A
#'
#'@example examples/cyclomortFit_example.R
#'@export

fit_cyclomort = function(T, p0, dt, period) {
  optim(p0, loglike_optim, T = T, dt = dt, period = period, hessian = TRUE);
}