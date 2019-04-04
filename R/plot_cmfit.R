#' Plot cmfit objects
#' 
#' @param x a cmfit object
#' 
#' @return a plot comparing the estimated mortality curve (based on parameter estimates)
#' and the actual results (as a histogram).
#' 
#' @example examples/cyclomortFit_example.R
#' @export

plot.cmfit = function(x, n.times = 1e3, type = c("hazard", "mortality"),  ...) {
  uncensoredData = as.numeric(x$data[x$data[,2] == 1,1]) ##uncensored data
  
  if(length(type) == 2) par(mfrow = c(1,2))
  
  if("hazard" %in% type){
    timeOfYearData = uncensoredData - floor(uncensoredData)
    h = hist(timeOfYearData, xlab = "Time within a period", ylab = "Relative risk of mortality",
             main = "Comparing hazard function estimate with actual mortality data", 
             col = "grey", bor = "darkgrey", freq = FALSE, ... )
    
    plotHazard(x, add = TRUE)
  }
  
  if("mortality" %in% type){
  
    h = hist(uncensoredData, xlab = "Number of periods", ylab = "Number of mortalities",
         main = "Comparing parameter estimates with actual mortality data", 
          col = "grey", bor = "darkgrey", ...)
    
    period = x$period
    t <- seq(0, max(uncensoredData), length = n.times)
    
    rawEstimates = fits$optim$par
    
    mus = rawEstimates[grepl("mu", names(rawEstimates))] * period
    rhos = rawEstimates[grepl("rho", names(rawEstimates))]
    gammas = rawEstimates[grepl("gamma", names(rawEstimates))] / period
    
    hazard = mwc(t * period, mus, rhos, gammas, period)
    cumhazard = imwc(t * period, mus, rhos, gammas, period)
    cum.prob.survival <-  exp(-cumhazard)
    cum.mortality <- 1 - cum.prob.survival
    pdf.mortality <- hazard * cum.prob.survival
    adj.pdf.mortality = pdf.mortality / max(pdf.mortality) * max(h$counts)
    
    lines(t, adj.pdf.mortality)
  }
}

#' Plots the hazard curve for one period given either a cmfit object or a set of parameters
#' 
#' to do!!
#' 
#' @export

plotHazard = function(cmfit = NULL, pars, period, ... ) {
  if (!is.null(cmfit)) {
    pars = fits$optim$par
    period = cmfit$period
  }
  
  rhos = pars[grepl("rho", names(pars))]
  mus = pars[grepl("mu", names(pars))]
  gammas = pars[grepl("gamma", names(pars))]
  
  if (is.null(cmfit)) {
    mus = mus / period
    gammas = gammas * period
  }
  
  thisHazard = function(x) {
    mwc(t = x, mus = mus, rhos = rhos, gammas = gammas, tau = 1)
  }
  
  curve(thisHazard, xlim = c(0, 1), ... )
}