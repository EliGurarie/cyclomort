#'Summary method for cmfitlist objects
#'
#'@param object a \code{cmfitlist} object - output of \code{\link{select_seasons}}
#'@param ... (currently not implemented)
#'@param coefs whether or not to return model coefficients along with statistic test table.
#'
#'@return a data frame describing the AIC, log-likelihood, number of parameters and parameter estimates for each model
#'
#'@example examples/select_seasons_example.R
#'@export


summary.cmfitlist = function(object, ..., coefs = TRUE) {
  if (coefs) {
    estimates = ldply(object, function(l) cbind(l$n.seasons, 
                                                rbind(ldply(l$estimates))))
    names(estimates)[1] = "n.seasons"
    estimates$parameter[is.na(estimates$parameter)] = "meanhazard"
    values = ldply(object, function(x) x$AIC) %>% rename(c(V1 = "AIC"))
    return(list(estimates = estimates, AICtable = values))
  }
  summarize_listOfFits(object, lrt = TRUE, print = FALSE)
}


#' @export
print.cmfitlist = function(x, ...) {
  print(summary(x, ...))
}

