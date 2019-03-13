#' Identify the ideal number of peaks for a set of mortality data
#' 
#' Obtain AIC values for the MLEs for a two-peak, three-peak and four-peak mortality model.
#' Decide which model should be used based on the lowest AIC; this will estimate how many
#' peaks in mortality occur during a periodic cycle for the given species.
#' 
#' @param T set of Surv objects representing time of death or censorship
#' @param dt interval for plots as well as precision of random samples
#' 
#' @return a cmfit object with the most likely number of periodicities
#' 
#' @example examples/getIdealPeakFit_example.R
#' 
#' @export

getIdealPeakFit = function(T, dt) {
  ##null parameter estimates should vary a little given how many peaks there are
  p0_twoPeaks = c(A = 0.05, peak1 = 0.25, peak2 = 0.75, rho1 = 0.5, rho2 = 0.5, weight1 = 0.5)
  p0_threePeaks = c(A = 0.05, peak1 = 0.25, peak2 = 0.50, peak3 = 0.75, rho1 = 0.5, rho2 = 0.5, rho3 = 0.5, weight1 = 1/3, weight2 = 1/3)
  p0_fourPeaks = c(A = 0.05, peak1 = 0.2, peak2 = 0.4, peak3 = 0.6, peak4 = 0.8, rho1 = 0.5, rho2 = 0.5, rho3 = 0.5, rho4 = 0.5, weight1 = 0.25, weight2 = 0.25, weight3 = 0.25)
  
  twoPeakFit = fit_cyclomort(T = T, p0 = p0_twoPeaks, dt = dt)
  threePeakFit = fit_cyclomort(T = T, p0 = p0_threePeaks, dt = dt)
  fourPeakFit = fit_cyclomort(T = T, p0 = p0_fourPeaks, dt = dt)
  
  twoPeakAIC = twoPeakFit$AIC
  threePeakAIC = threePeakFit$AIC
  fourPeakAIC = fourPeakFit$AIC
  
  bestAIC = min(twoPeakAIC, threePeakAIC, fourPeakAIC)
  
  if (bestAIC == twoPeakAIC) {
    return (twoPeakFit)
  } else if (bestAIC == threePeakAIC) {
    return (threePeakFit)
  } else {##if (bestAIC == fourPeakAIC)
    return (fourPeakFit)
  }
}