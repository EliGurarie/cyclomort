#' Identify the ideal number of seasons for a set of mortality data
#' 
#' Obtain AIC values for parameter estimates for a number of different models, based on the number of predicted seasons for a set of survival objects. This method will identify the most likely number of "mortality seasons" in one period for a species.
#' 
#' @param T set of Surv objects representing time of death or censorship
#' @param max.season maximum number of seasons to survey
#' @param method method used to evaluate model accuracy - either AIC or BIC
#' 
#' @return a cmfitlist object comparing the various fits
#' 
#' @example examples/getIdealPeakFit_example.R
#' 
#' @export

selectNSeasons = function(T, max.season = 4, method = "AIC") {
  
  listOfFits = list()
  n.seasons = 0
  while (n.seasons <= max.season) {
    listOfFits[[n.seasons+1]] = fit_cyclomort(T, n.seasons = n.seasons)
    names(listOfFits)[n.seasons + 1] = paste0("seasons", n.seasons)
    n.seasons = n.seasons + 1
  }
  
  class(listOfFits) = "cmfitlist"
  attributes(listOfFits)$method = method
  listOfFits
}
