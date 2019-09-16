#' Summary method for cyclomort factorial fit
#' 
#' @param object a \code{cmfactorfit} object - the output of \code{\link{factorfit_cyclomort}}.
#' @param ... (not implemented) 
#' @param coefs whether or not to report the individual summaries of each model component along with the statistical test results
#' 
#' @return a table comparing log-likelihood and AIC between null and multi-factor model, and a p-value from likelihood ratio test, optionally combined with the individual model summaries. 
#' 
#' @example examples/factorfit_cyclomort_example.R
#' @export

summary.cmfactorfit = function(object, ..., coefs = FALSE) {
  f <- paste(as.character(object$formula)[c(2,1,3)],collapse = " ")
  cat(paste0("Summary table comparing factorial seasonal survival model with ", object$n.seasons, " seasons.\n\nFormula: ", f, "\n"))
  df <- data.frame(logLike = object$ll %>% unlist,
                   k = object$k %>% unlist %>% signif(5),
                   AIC = object$aic %>% unlist %>% signif(5),
                   LRT = c(round(object$lrt,2), ""),
                   p.value = c(round(object$p.value,3), ""))
  
  row.names(df)[2] <- as.character(object$formula)[3]
  if (!coefs) return(df)
  
  result = list(test = df)
  result$null = summary(object$fits$null)
  result = list(result, lapply(object$fits$alt, summary))
  result
}