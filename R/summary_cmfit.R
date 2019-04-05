#'Provides a short summary of the cmfitlist object, displaying the effectiveness of each model
#'
#'@param x a cmfitlist object
#'
#'@return a data frame describing the AIC, log-likelihood, number of parameters and parameter estimates for each model
#'
#'@example examples/getIdealPeakFit_example.R
#'@export


summary.cmfitlist = function(x) {
  ldply(listOfFits, function(l) rbind(ldply(l$estimates)))
  print(ldply(listOfFits, AIC) %>% rename(c(V1 = "AIC")))
}