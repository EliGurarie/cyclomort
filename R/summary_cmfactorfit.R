#' Produce a table explaining the results of a cmfactorfit model
#' 
#' @param x a cmfactorfit object
#' 
#' @return a table comparing log-likelihood and AIC between null and multi-factor model, as well as a p-value from likelihood ratio test
#' 
#' @example examples/factorfit_cyclomort_example.R
#' @export

summary.cmfactorfit = function(x) {
  f <- paste(as.character(x$formula)[c(2,1,3)],collapse = " ")
  cat(paste0("Summary table comparing factorial seasonal survival model with ", x$n.seasons, " seasons.\n\nFormula: ", f, "\n"))
  df <- data.frame(logLike = x$ll %>% unlist,
                   k = x$k %>% unlist %>% signif(5),
                   AIC = x$aic %>% unlist %>% signif(5),
                   LRT = c(round(x$lrt,2), ""),
                   p.value = c(round(x$p.value,3), ""))
  
  row.names(df)[2] <- as.character(x$formula)[3]
  df
}




