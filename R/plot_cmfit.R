#' Plot cmfit objects
#' 
#' @param x a cmfit object
#' 
#' @return a plot comparing the estimated mortality curve (based on parameter estimates)
#' and the actual results (as a histogram).
#' 
#' @example examples/cyclomortFit_example.R
#' @export

plot.cmfit = function(x, n.times = 1e3,  ...) {
  
  par(mar = c(4,4,4,4))
  uncensoredData = as.numeric(x$data[x$data[,3] == 1,2]) ##uncensored data
 
  timeOfYearData = uncensoredData - floor(uncensoredData)
    h = hist(timeOfYearData, xlab = "Time within a period", ylab = "Number of mortalities",
             main = "", col = "grey", bor = "darkgrey", freq = TRUE, ... )
    plotHazard(x)
  
  mtext("Comparing parameter estimates with actual mortality data", font = 2, side = 3, line = -2.5, outer = TRUE)
  
}

#' Plots the hazard curve for one period given either a cmfit object or a set of parameters
#' 
#' to do!!
#' 
#' @export

plotHazard = function(cmfit = NULL, pars, period) {
  
  if(cmfit$n.seasons == 0) abline(h = cmfit$estimates$meanhazard[1,1]) else
  {
    if (!is.null(cmfit)) {
      pars = cmfit$optim$par
      period = cmfit$period
    }
    
    lrhos = pars[grepl("lrho", names(pars))]
    mus = pars[grepl("mu", names(pars))]
    gammas = pars[grepl("gamma", names(pars))]
    
    t <- seq(0, 1, length = 1e4)
    hazard <- mwc(t = t, mus = mus, rhos = expit(lrhos), gammas = gammas, tau = 1)
    
    K <- par("usr")[4]  # find the dimension of the plotting window

    hazard.labs <- pretty(hazard)
    axis(4, at = hazard.labs * K / max(hazard), hazard.labs, las = 2)
    
    lines(t, hazard*K/max(hazard), lwd = 2)
    mtext(side = 4, line = 2.5, "Estimated hazard function")
  }
}

  
