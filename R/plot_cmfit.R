#' Plot cmfit objects
#' 
#' @param x a cmfit object
#' 
#' @return a plot comparing the estimated mortality curve (based on parameter estimates)
#' and the actual results (as a histogram).
#' 
#' @example examples/cyclomortFit_example.R
#' @export

plot.cmfit = function(x, CI = TRUE, nreps = 1e4,  ...) {
  
  # tweak margins to fit right legend
  mars <- par()$mar
  mars[4] <- max(par()$mar[4], 4.1)
  par(mar = mars, bty = "u")
    
  uncensoredData = as.numeric(x$data[x$data[,3] == 1,2]) ##uncensored data
  timeOfYearData = uncensoredData - floor(uncensoredData)
  h = hist(timeOfYearData, xlab = "Time within a period", ylab = "Number of mortalities",
           main = "", col = "grey", bor = "darkgrey", freq = TRUE, ... )

  predict.hazard <- predict.cmfit(x, CI = CI, nreps = nreps)
  
  K <- par("usr")[4]/max(predict.hazard$CI)  # find the dimension of the plotting window
  
  hazard.labs <- pretty(predict.hazard$fit)
  axis(4, at = hazard.labs * K , hazard.labs, las = 2)
  mtext(side = 4, line = 2.5, "Estimated hazard function")
  
  with(predict.hazard, {
      lines(t, fit * K, lwd = 2)
      lines(t, CI[1,] * K, lty = 3)
      lines(t, CI[2,] * K, lty = 3)
  })
  mtext(side = 4, line = 2.5, "Estimated hazard function")
}

