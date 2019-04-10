#'Provides a short summary of the cmfitlist object, displaying the effectiveness of each model
#'
#'@param x a cmfitlist object
#'
#'@return a data frame describing the AIC, log-likelihood, number of parameters and parameter estimates for each model
#'
#'@example examples/getIdealPeakFit_example.R
#'@export


summary.cmfitlist = function(x) {
  estimates = ldply(x, function(l) cbind(l$n.seasons, rbind(ldply(l$estimates))))
  names(estimates)[1] = "n.seasons"
  estimates$parameter[is.na(estimates$parameter)] = "meanhazard"
  method = attributes(x)$method
  if (method == "AIC") {
    values = ldply(x, AIC) %>% rename(c(V1 = "AIC"))
  } else {
    # if (method == "BIC")
    values = ldply(x, BIC) %>% rename(c(V1 = "BIC"))
  }
  list(estimates = estimates, model.accuracy = values)
}