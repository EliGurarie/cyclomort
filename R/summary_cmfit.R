#'Provides a short summary of the cmfitlist object, displaying the effectiveness of each model
#'
#'@param x a cmfitlist object
#'
#'@return a data frame describing the AIC, log-likelihood, number of parameters and parameter estimates for each model
#'
#'@example examples/getIdealPeakFit_example.R
#'@export


summary.cmfitlist = function(x) {
  numModels = length(x)
  result = data.frame()
  result = rbind(result, c(0, AIC(x$zeroSeason), logLik(x$zeroSeason), 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  if (numModels > 1) {
    result = rbind(result, c(1, AIC(x$oneSeason), logLik(x$oneSeason), 3, x$oneSeason$peak1[1], NA, NA, NA, x$oneSeason$duration1[1], NA, NA, NA, NA, NA, NA, x$oneSeason$meanhazard[1]))
    if (numModels > 2) {
      result = rbind(result, c(1, AIC(x$twoSeason), logLik(x$twoSeason), 6, x$twoSeason$peak1[1], x$twoSeason$peak2[1], NA, NA, x$twoSeason$duration1[1], x$twoSeason$duration2[1], NA, NA, x$twoSeason$weight1[1], NA, NA, x$twoSeason$meanhazard[1]))
      if (numModels > 3) {
        result = rbind(result, c(1, AIC(x$threeSeason), logLik(x$threeSeason), 9, x$threeSeason$peak1[1], x$threeSeason$peak2[1], x$threeSeason$peak3[1], NA, x$threeSeason$duration1[1], x$threeSeason$duration2[1], x$threeSeason$duration3[1], NA, x$threeSeason$weight1[1], x$threeSeason$weight2[1], NA, x$threeSeason$meanhazard[1]))
        if (numModels > 4) {
          result = rbind(result, c(1, AIC(x$fourSeason), logLik(x$fourSeason), 12, x$fourSeason$peak1[1], x$fourSeason$peak2[1], x$fourSeason$peak3[1], x$fourSeason$peak4[1], x$fourSeason$duration1[1], x$fourSeason$duration2[1], x$fourSeason$duration3[1], x$fourSeason$duration4[1], x$fourSeason$weight1[1], x$fourSeason$weight2[1], x$fourSeason$weight3[1], x$fourSeason$meanhazard[1]))
        }
      }
    }
  }
  names(result) = c("numSeasons", "AIC", "logLik", "k", "peak1", "peak2", "peak3", "peak4", "duration1", "duration2", "duration3", "duration4", "weight1", "weight2", "weight3", "meanhazard")
  result
}