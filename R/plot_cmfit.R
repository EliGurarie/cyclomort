#' Plot cmfit objects
#' 
#' @param x a cmfit object
#' @param CI boolean variable determining whether confidence intervals are included in the hazard function estimate
#' @param histogram boolean variable determining whether a histogram will be included with the hazard
#' @param add boolean variable determining whether the plot will be added to an existing plot
#' @param nreps number of repetitions of random parameter sampling; used to develop confidence intervals
#' @param hazcolor color of lines for hazard function and confidence intervals
#' @param alpha transparency of confidence interval polygon
#' @param ymax maximum value for the y-axis - can be useful for scaling purposes
#' @param prediction an optional \code{\link{prediction.cmfit}} object- otherwise the function will estimate this every time which can be a bit slow. 
#' 
#' @return a plot comparing the estimated mortality curve (based on parameter estimates)
#' and the actual results (as a histogram).
#' 
#' @example examples/cyclomortFit_example.R
#' @export

plot.cmfit = function(x, CI = TRUE, histogram = TRUE, add = FALSE, 
                      nreps = 1e4, hazcolor = "black", alpha = 0.3, 
                      ymax = NULL, prediction = NULL, yaxt = par()$yaxt, ...) {

  if(is.null(prediction)){
    prediction <- predict(x, CI = CI, nreps = nreps, type = "hazard")
    prediction$t <- prediction$t / x$period
    CI <- !is.null(prediction$CI)
  }

  if(is.null(ymax))  ymax <- with(prediction, max(fit, CI), na.rm = TRUE)
  
  uncensoredData = as.numeric(x$data[x$data[,3] == 1,2]) ##uncensored data
  timeOfYearData = uncensoredData - floor(uncensoredData)
  
  if(histogram) {
    # tweak margins to fit right legend
    mars <- par()$mar
    mars[4] <- max(par()$mar[4], 4.1)
    par(mar = mars, bty = "u")
    
    h = hist(timeOfYearData, 
             xlab = "Time within a period", ylab = "Number of mortalities",
             main = "", col = "grey", bor = "darkgrey", freq = TRUE,
             add = add, plot = histogram, ... )
    
   }
  
  K <- ifelse(histogram, par("usr")[4]/ymax, 1)  # find the dimension of the plotting window
  
  if(!histogram && !add) {
    #need to make a new plot window!
    plot.new()
    xmax = par("xaxp")[2]
    plot.window(xlim = c(0, xmax), ylim = c(0, ymax))
    asp = par("xaxp")[3]
    axis(side = 1, at = 0:(asp * xmax) / asp, labels = 0:asp / asp)
    mtext("Time within a period", side = 1, at = xmax / 2, line = 2)
  }
  
  hazard.labs <- pretty(prediction$CI)
  if (!add) {
    # if we don't have a histogram our hazard axis should be on the left!
    if(yaxt != "n")  axis(ifelse(histogram, 4, 2), at = hazard.labs * K, hazard.labs)
    mtext(side = ifelse(histogram, 4, 2), line = 2.5, "Estimated hazard function")
  }
  
  if (CI) {
    with(prediction, {
      lines(t, fit*K, col = hazcolor, lwd = 2)
      t.poly <- c(t, t[length(t):1])
      CI.poly <- c(CI[1,], CI[2,length(t):1])*K
      polygon(t.poly, CI.poly, col = alpha(hazcolor, alpha), bor = NA)
      lines(t, CI[1,]*K, col = hazcolor, lty = 3)
      lines(t, CI[2,]*K, col = hazcolor, lty = 3)
    })
  } else {
    with(prediction, {
      lines(t, fit, col = hazcolor, lwd = 2)
    })
  }
}