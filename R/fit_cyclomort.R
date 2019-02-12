#'Get maximum likelihood estimates for periodic mortality data
#'
#'@export

fit_cyclomort = function(T, p0, dt, period) {
  optim(p0, loglike_optim, T = T, dt = dt, period = period, hessian = TRUE);
}