#' Plot cmfit objects
#' 
#' @param x a cmfit object
#' @param CI boolean variable determining whether confidence intervals are included in the hazard function estimate
#' @param histogram boolean variable determining whether a histogram will be included with the hazard
#' @param add boolean variable determining whether the plot will be added to an existing plot
#' @param nreps number of repetitions of random parameter sampling; used to develop confidence intervals
#' @param hazcolor color of lines for hazard function and confidence intervals
#' 
#' @return a plot comparing the estimated mortality curve (based on parameter estimates)
#' and the actual results (as a histogram).
#' 
#' @example examples/cyclomortFit_example.R
#' @export

plot.cmfit = function(x, CI = TRUE, histogram = TRUE, add = FALSE, 
                      nreps = 1e4, hazcolor = "black", alpha = 0.5,  ...) {
  
  if(histogram) {
    # tweak margins to fit right legend
    mars <- par()$mar
    mars[4] <- max(par()$mar[4], 4.1)
    par(mar = mars, bty = "u")
  }
    
  uncensoredData = as.numeric(x$data[x$data[,3] == 1,2]) ##uncensored data
  timeOfYearData = uncensoredData - floor(uncensoredData)
  
  h = hist(timeOfYearData, xlab = "Time within a period", ylab = "Number of mortalities",
           main = "", col = "grey", bor = "darkgrey", freq = TRUE,
           add = add, plot = histogram, ... )
  
  if(!histogram && !add) {
    #need to make a new plot window!
    plot.new()
    xmax = par("xaxp")[2]
    plot.window(xlim = c(0, xmax), ylim = c(0, 1))
    asp = par("xaxp")[3]
    axis(side = 1, at = 0:(asp * xmax) / asp, labels = 0:asp / asp)
    mtext("Time within a period", side = 1, at = xmax / 2, line = 2)
  }

  predict.hazard <- predict(x, CI = CI, nreps = nreps, type = "hazard")
  predict.hazard$t <- predict.hazard$t / x$period
  
  K <- par("usr")[4]/max(predict.hazard$CI[2,])  # find the dimension of the plotting window
  
  hazard.labs <- pretty(predict.hazard$CI)
  if (!add) {
    # if we don't have a histogram our hazard axis should be on the left!
    axis(ifelse(histogram, 4, 2), at = hazard.labs * K , hazard.labs, las = 2)
    mtext(side = ifelse(histogram, 4, 2), line = 2.5, "Estimated hazard function")
  }
  
  if (CI) {
  with(predict.hazard, {
      lines(t, fit * K, col = hazcolor, lwd = 2)
      lines(t, CI[1,] * K, col = hazcolor, lty = 3)
      lines(t, CI[2,] * K, col = hazcolor, lty = 3)
      t.poly <- c(t, t[length(t):1])
      CI.poly <- c(CI[1,], CI[2,length(t):1])
      polygon(t.poly, CI.poly, col = alpha(hazcolor, alpha), bor = NA)
  })
  } else {
    with(predict.hazard, {
      lines(t, fit * K, col = hazcolor, lwd = 2)
    })
  }
}

