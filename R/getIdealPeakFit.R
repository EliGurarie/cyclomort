#' Identify the ideal number of seasons for a set of mortality data
#' 
#' Obtain AIC values for the MLEs for a five different mortality models, each one having a different number of seasons.
#' Decide which model should be used based on the lowest AIC; this will estimate how many
#' seasons in mortality occur during a periodic cycle for the given species.
#' 
#' @param T set of Surv objects representing time of death or censorship
#' @param dt interval for plots as well as precision of random samples
#' 
#' @return a cmfitlist object comparing the various fits
#' 
#' @example examples/getIdealPeakFit_example.R
#' 
#' @export

selectNSeasons = function(T, dt = 0.01) {
  p0_noPeaks = c(A = 0.05, peak1 = 0.5, rho1 = 0) # rho == 0 indicates the null model
  p0_onePeak = c(A = 0.05, peak1 = 0.5, rho1 = 0.5)
  p0_twoPeaks = c(A = 0.05, peak1 = 0.25, peak2 = 0.75, rho1 = 0.5, rho2 = 0.5, weight1 = 0.5)
  p0_threePeaks = c(A = 0.05, peak1 = 1/6, peak2 = 0.5, peak3 = 5/6, rho1 = 0.5, rho2 = 0.5, rho3 = 0.5, weight1 = 1/3, weight2 = 1/3)
  p0_fourPeaks = c(A = 0.05, peak1 = 1/8, peak2 = 3/8, peak3 = 5/8, peak4 = 7/8, rho1 = 0.5, rho2 = 0.5, rho3 = 0.5, rho4 = 0.5, weight1 = 0.25, weight2 = 0.25, weight3 = 0.25, weight4 = 0.25)
  
  listOfFits = list()
  
  nullModelFit = fit_cyclomort(T = T, p0 = p0_noPeaks, dt = dt)
  listOfFits$null = nullModelFit
  
  onePeakFit = fit_cyclomort(T = T, p0 = p0_onePeak, dt = dt)
  if (AIC(nullModelFit) >= AIC(onePeakFit)) {
    listOfFits$onePeak = onePeakFit
    twoPeakFit = fit_cyclomort(T = T, p0 = p0_twoPeaks, dt = dt)
    if (AIC(onePeakFit) >= AIC(twoPeakFit)) {
      listOfFits$twoPeak = twoPeakFit
      threePeakFit = fit_cyclomort(T = T, p0 = p0_threePeaks, dt = dt)
      if (AIC(twoPeakFit) >= AIC(threePeakFit)) {
        listOfFits$threePeak = threePeakFit
        fourPeakFit = fit_cyclomort(T = T, p0 = p0_fourPeaks, dt = dt)
        listOfFits$fourPeak = fourPeakFit
      }
    }
  }
  
  listOfFits$dt = dt
  class(listOfFits) = "cmfitlist"
  listOfFits
}
