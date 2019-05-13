#'Provides a short summary of the cmfitlist object, displaying the effectiveness of each model
#'
#'@param x a cmfitlist object
#'@param coefs prints out list of model coefficient estimates if true, otherwise prints out simple AIC table comparing models
#'@param plotme plots AIC/BIC values from each cmfit object in the list to identify the best model choice
#'
#'@return a data frame describing the AIC, log-likelihood, number of parameters and parameter estimates for each model
#'
#'@example examples/getIdealPeakFit_example.R
#'@export


summary.cmfitlist = function(x, coefs = TRUE, plotme = FALSE) {
  if (coefs) {
    estimates = ldply(x, function(l) cbind(l$n.seasons, rbind(ldply(l$estimates))))
    names(estimates)[1] = "n.seasons"
    estimates$parameter[is.na(estimates$parameter)] = "meanhazard"
    values = ldply(x, AIC) %>% rename(c(V1 = "AIC"))
    if (plotme) {
      plot(0:(length(x) - 1), values[,2], xlab = "Number of seasons", ylab = "Akaike Information Criterion (AIC)")
    }
    return(list(estimates = estimates, model.accuracy = values))
  }
  summarize_listOfFits(x, lrt = TRUE, print = FALSE)
}