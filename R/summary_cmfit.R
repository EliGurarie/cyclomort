#'Provides a short summary of the cmfitlist object, displaying the effectiveness of each model
#'
#'@param x a cmfitlist object
#'
#'@return a data frame describing the AIC, log-likelihood, number of parameters and parameter estimates for each model
#'
#'@example examples/getIdealPeakFit_example.R
#'@export


summary.cmfitlist = function(x) {
  numModels = length(x) - 1 # last item of the list is dt
  result = data.frame()
  result = rbind(result, c(0, AIC(x$null), logLik(x$null), 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  if (numModels > 1) {
    result = rbind(result, c(1, AIC(x$onePeak), logLik(x$onePeak), 3, x$onePeak$peak1[1], NA, NA, NA, x$onePeak$rho1[1], NA, NA, NA, NA, NA, NA, x$onePeak$A[1]))
    if (numModels > 2) {
      result = rbind(result, c(1, AIC(x$twoPeak), logLik(x$twoPeak), 6, x$twoPeak$peak1[1], x$twoPeak$peak2[1], NA, NA, x$twoPeak$rho1[1], x$twoPeak$rho2[1], NA, NA, x$twoPeak$weight1[1], NA, NA, x$twoPeak$A[1]))
      if (numModels > 3) {
        result = rbind(result, c(1, AIC(x$threePeak), logLik(x$threePeak), 9, x$threePeak$peak1[1], x$threePeak$peak2[1], x$threePeak$peak3[1], NA, x$threePeak$rho1[1], x$threePeak$rho2[1], x$threePeak$rho3[1], NA, x$threePeak$weight1[1], x$threePeak$weight2[1], NA, x$threePeak$A[1]))
        if (numModels > 4) {
          result = rbind(result, c(1, AIC(x$fourPeak), logLik(x$fourPeak), 12, x$fourPeak$peak1[1], x$fourPeak$peak2[1], x$fourPeak$peak3[1], x$fourPeak$peak4[1], x$fourPeak$rho1[1], x$fourPeak$rho2[1], x$fourPeak$rho3[1], x$fourPeak$rho4[1], x$fourPeak$weight1[1], x$fourPeak$weight2[1], x$fourPeak$weight3[1], x$fourPeak$A[1]))
        }
      }
    }
  }
  names(result) = c("numSeasons", "AIC", "logLik", "k", "peak1", "peak2", "peak3", "peak4", "rho1", "rho2", "rho3", "rho4", "weight1", "weight2", "weight3", "A")
  result
}