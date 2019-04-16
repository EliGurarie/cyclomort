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
  uncensoredData = as.numeric(x$data[x$data[,3] == 1,2]) ##uncensored data
  
  if(length(type) == 2) par(mfrow = c(1,2))
  
  if("hazard" %in% type){
    timeOfYearData = uncensoredData - floor(uncensoredData)
    h = hist(timeOfYearData, xlab = "Time within a period (starting at given phase value)", ylab = "Relative risk of mortality",
             main = "", col = "grey", bor = "darkgrey", freq = FALSE, ... )
    
    plotHazard(x, hist = h, add = TRUE)
  }
  
  if("mortality" %in% type){
  
    h = hist(uncensoredData, xlab = "Number of periods", ylab = "Number of mortalities",
             main = "", col = "grey", bor = "darkgrey", ...)
    
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
  
  mtext("Comparing parameter estimates with actual mortality data", font = 2, side = 3, line = -2.5, outer = TRUE)
  
}

#' Plots the hazard curve for one period given either a cmfit object or a set of parameters
#' 
#' to do!!
#' 
#' @export

plotHazard = function(cmfit = NULL, hist, pars, period, ... ) {
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
  
  maxValue = max(thisHazard(seq(0, 1, 0.01))) #is there a better way to do this?
  
  modifiedHazard = function(x) {
    thisHazard(x) / maxValue * max(hist$density)
  }
  
  curve(modifiedHazard, xlim = c(0, 1), ... )
}