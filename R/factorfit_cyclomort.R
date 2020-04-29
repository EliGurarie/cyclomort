#' Factorial analysis of seasonal survival models
#' 
#' This function takes a \code{Y~X} style formula to compare null models of 
#' pooled data against separately fitted models against a given factor.  For now 
#' this works only for a single discrete factor. 
#' 
#' @param f formula object used for identifying different classes
#' @param data a data frame containing a cycloSurv object detailing mortalities 
#' for a set of observations and a factor identifying the value of a categorical 
#' variable for each observation
#' @param n.seasons number of seasons to fit model to
#' @param ... additional arguments to fit_cyclomort call
#' 
#' @return table comparing outputs from null (factor has no effect on mortality 
#' and they are all in the same group) model to multi-factor model using AIC, 
#' log-likelihood and likelihood ratio test
#' 
#' @example examples/factorfit_cyclomort_example.R
#' @export

factorfit_cyclomort = function(f, data = NULL, n.seasons = 2, ... ) {
  
  if(!is.null(data)){
    t <- data[,as.character(f)[2]]
    x <- data[,as.character(f)[3]]
  } else {
    t <- get(as.character(f)[2])
    x <-  get(as.character(f)[3])
  }
  
  fit_null = fit_cyclomort(t, n.seasons = n.seasons, ... )
  #L = list(null = nullModelFits)
  
  ll_null = logLik(fit_null)
  AIC_null = fit_null$AIC
  k_null <- max(1, 3*n.seasons)
  
  x.levels = levels(droplevels(x))
  n.factors <- length(x.levels)
  
  fits_alt <- list() 
  
  for (i in 1:length(x.levels)) {
    t.subset = t[x == x.levels[i]]
    attributes(t.subset)[c("period","t0")] <- attributes(t)[c("period","t0")]
    fits_alt[[i]] = fit_cyclomort(t.subset, n.seasons = n.seasons, ... )
    names(fits_alt)[i] = x.levels[i]
  }

  ll_alt <- sapply(fits_alt, logLik) %>% sum
  k_alt <- 3*n.seasons*n.factors
  AIC_alt <- -2*ll_alt + 2*k_alt
    
  lrt <- 2*(ll_alt - ll_null)
  p_lrt = 1 - pchisq(lrt, k_alt - k_null)
  
  L <- list(formula = f,
            n.seasons = n.seasons, 
            fits = list(null = fit_null, alt = fits_alt), 
            ll = list(null = ll_null, alt = ll_alt),
            aic = list(null = AIC_null, alt = AIC_alt), 
            k = list(null = k_null, alt = k_alt), 
            lrt = lrt, p.value = p_lrt)
  class(L) = "cmfactorfit"
  L
}



