#' Identify the ideal number of seasons for a set of mortality data
#' 
#' Obtain AIC values for the MLEs for a five different mortality models, each one having a different number of seasons.
#' Decide which model should be used based on the lowest AIC; this will estimate how many
#' seasons in mortality occur during a periodic cycle for the given species.
#' 
#' @param T set of Surv objects representing time of death or censorship
#' 
#' @return a cmfitlist object comparing the various fits
#' 
#' @example examples/getIdealPeakFit_example.R
#' 
#' @export

selectNSeasons = function(T) {
  listOfFits = list()
  
  nullFit = fit_cyclomort(T, n.seasons = 0)
  listOfFits$zeroSeason = nullFit
  oneFit = fit_cyclomort(T, n.seasons = 1)
  if (AIC(nullFit) >= AIC(oneFit)) {
    listOfFits$oneSeason = oneFit
    twoFit = fit_cyclomort(T, n.seasons = 2)
    if (AIC(oneFit) >= AIC(twoFit)) {
      listOfFits$twoSeason = twoFit
      threeFit = fit_cyclomort(T, n.seasons = 3)
      if (AIC(twoFit) >= AIC(threeFit)) {
        listOfFits$threeSeason = threeFit
        fourFit = fit_cyclomort(T, n.seasons = 4)
        if (AIC(threeFit) >= AIC(fourFit)) {
          listOfFits$fourSeason = fourFit
        }
      }
    }
  }
  
  class(listOfFits) = "cmfitlist"
  listOfFits
}
