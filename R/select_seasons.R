#' Select the number of  mortality seasons
#' 
#' Compute a delta AIC table (and, optionally, likelihood ratio tests) for a sequence of models with a different number of seasons
#' 
#' @param x \code{\link{create_cycloSurv}} object
#' @param max.season maximum number of seasons to fit
#' @param lrt whether or not to perform and return the complete results of nested likelihood ratio tests
#' @param print boolean parameter; if TRUE the function prints the table out as a side effect of creating the object
#' 
#' @return a list containing (1) a list of all the fitted objects, and (2) an AIC (and, optionally, LRT) summary table.  Also prints both tables by default.  
#' 
#' @example examples/select_seasons_example.R
#' 
#' @export

select_seasons = function(x, max.season = 4, lrt = FALSE, print = TRUE) {
  
  listOfFits = list()
  n.seasons = 0
  while (n.seasons <= max.season) {
    message(paste("Fitting model with", n.seasons, "seasons...\n"))
    listOfFits[[n.seasons+1]] = fit_cyclomort(x, n.seasons = n.seasons)
    names(listOfFits)[n.seasons + 1] = paste0("fit",n.seasons)
    n.seasons = n.seasons + 1
  }
  
  lof.summary <- summarize_listOfFits(listOfFits, lrt = lrt, print = print)
  class(listOfFits) = "cmfitlist"
  return(list(fits = listOfFits, summary = lof.summary))
}


summarize_listOfFits <- function(listOfFits, lrt = lrt, print = print){
  AIC.table <- ldply(listOfFits, summarize, 
                     n.seasons = n.seasons, 
                     logLik = logLik, 
                     d.f. = ifelse(n.seasons == 0, 1, n.seasons * 3), 
                     AIC = AIC) %>% mutate(dAIC = AIC - min(AIC), .id = NULL)
  
  n.seasons <- length(listOfFits) - 1
  if(print){
    cat("\nDelta AIC table of fitted models:\n")
    print(AIC.table, row.names = FALSE)
  }
  
  if(lrt){
    fits.ll <- AIC.table$logLik %>% as.numeric
    ks <- AIC.table$d.f.
    
    chisq.vals <- (outer(fits.ll, fits.ll, `-`) %>% abs) * 2
    dfs <- outer(ks, ks, `-`) %>% abs
    p.vals <- 1-pchisq(chisq.vals, ks)
    mat.names <- outer(0:n.seasons, 0:n.seasons, paste, sep = "-")
    
    ut <- upper.tri(mat.names)
    LRT.table <- data.frame( comparison = mat.names[ut],
                             ChiSq = chisq.vals[ut] %>% round(2),
                             d.f. = dfs[ut],
                             p.value = p.vals[ut] %>%  signif(3)) %>% 
      arrange(comparison) %>% 
      mutate(signif = cut(p.value, c(-1,0.001, 0.01, 0.05, 0.1, 1), 
                          labels = c("***", "**", "*", "-", "")))
    if(print){
      cat("\nNested likelihood-ratio tests:\n")
      print(LRT.table, row.names = FALSE)  
    }
  } else LRT.table <- NULL
  return(list(AIC.table = AIC.table, LRT.table = LRT.table))
}
