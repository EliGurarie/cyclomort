#'Obtain log-likelihood value from a data set given a set of parameter values
#'
#'@param T times of death or censoring as Surv objects
#'@param A average annual hazard
#'@param p k-vector of peaks
#'@param r k-vector of rhos (concentration parameters)
#'@param w (k-1)-vector of weights
#'@param period period of one mortality cycle
#'@param dt interval for plots as well as precision of random samples
#'
#'@value the maximum likelihood value for this set of data
#'
#'@example examples/loglike_example.R
#'@export

loglike <- function(T, A, p, r, w, dt, period) {
  
  hazard <- function(t, amplitude, peaks, rhos, weights, period, dt){
    tt <- t/period * 2*pi
    mus <- peaks/period * 2*pi
    DwrappedMultiCauchy(tt, amplitude, mus, rhos, weights) / dt
  }
  
  TT = matrix(T);
  D = TT[,2];
  TT = TT[,1];
    
  logcumhaz <- sapply(TT, function(t){
    t.total <- seq(0, t, dt)
    sum(1 - hazard(t.total, amplitude = A, peaks = p, rhos = r, weights = w, period, dt)) * dt
  })
  
  loghaz <- log(hazard(TT, amplitude = A, peaks = p, rho = r, weights = w, period, dt) + 1e-40)
  sum(loghaz * D + logcumhaz * (1-D))
}