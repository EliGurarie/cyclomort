#' Compare model fits based on a factor - infer the effect of a categorical variable on 
#' the parameter estimates produced by fit_cyclomort.
#' 
#' @param formula formula object used for identifying different classes
#' @param data a cycloSurv object recording start and end times as well as status (dead/censored) and the length of one full period
#' @param n.seasons number of seasons to fit model to
#' 
#' @return table comparing outputs from null (factor has no effect on mortality and they are all
#' in the same group) model to multi-factor model using AIC, log-likelihood and likelihood ratio test
#' 
#' @example examples/cyclomortModel_example.R
#' @export

model_cyclomort = function(formula, data, n.seasons = 2) {
  decisionmatrix = model.matrix(formula(paste0(as.character(formula)[1], as.character(formula)[3], " - 1")))
  nullModelFits = fit_cyclomort(data$surv, n.seasons = n.seasons)
  L = list(null = nullModelFits)
  ll_null = logLik(nullModelFits)
  AIC_null = AIC(nullModelFits)
  ll_alt = 0
  AIC_alt = 0
  factornames = levels(data$factor)
  for (i in 1:length(factornames)) {
    surv.factor = data$surv[data$factor == factornames[i]]
    attributes(surv.factor)[c("type", "class", "period", "t0")] = attributes(data$surv)[c("type", "class", "period", "t0")]
    colnames(surv.factor) = colnames(data$surv)
    fits = fit_cyclomort(T = surv.factor, n.seasons = n.seasons)
    L[[i + 1]] = fits
    names(L)[i+1] = factornames[i]
    ll_alt = ll_alt + logLik(fits)
    AIC_alt = AIC_alt + AIC(fits)
  }
  p_lrt = 1 - pchisq(2*(ll_alt - ll_null), (3 * n.seasons) * (ncol(decisionmatrix) - 1))
  L$ll_null = ll_null
  L$ll_alt = ll_alt
  L$aic_null = AIC_null
  L$aic_alt = AIC_alt
  L$p = p_lrt
  class(L) = "cmfactorfit"
  L
}