#' Plot cmfit objects
#' 
#' @param x a cmfit object
#' @param plotCI whether confidence intervals should also be drawn. 
#' @param CI.level confidence level (default 0.95) for CIs (if CI is TRUE)
#' @param histogram boolean dictating whether a histogram of actual mortalities will be included in the plot
#' @param add boolean dictating whether the plot will be added to an existing plot
#' @param monthlabs whether or not to label the x-axis with months - suitable for (common) annual seasonal data.  
#' If FALSE, labels are numeric within the period [0,1]
#' @param nreps number of samples from parameter estimates for confidence intervals (see \code{\link{predict.cmfit}})
#' @param hazcolor color of lines for hazard function and confidence intervals
#' @param alpha transparency of confidence interval polygon
#' @param ymax maximum value for the y-axis - can be useful for scaling purposes
#' @param prediction an optional \code{\link{predict.cmfit}} object- otherwise the function will estimate this every 
#' time which can be a bit slow. 
#' @param yaxt location for y-axis label
#' @param ... additional parameters to \code{\link{hist}} (e.g., number of breaks)
#' 
#' @return a plot comparing the estimated mortality curve (based on parameter estimates)
#' and the actual results (as a histogram).
#' 
#' @seealso predict.cmfit
#' 
#' @example examples/cyclomortFit_example.R
#' @export

plot.cmfit = function(x, plotCI = TRUE, CI.level = 0.95, histogram = TRUE, 
                      add = FALSE, monthlabs = FALSE,
                      nreps = 5e3, hazcolor = "black", alpha = 0.3, 
                      ymax = NULL, prediction = NULL, yaxt = par()$yaxt, ...) {
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  if(is.null(prediction)){
    prediction <- predict(x, CI = plotCI, CI.level = CI.level, nreps = nreps, 
                          type = "hazard")
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
             xlab = ifelse(monthlabs, "Time of year", "Time within a period"), 
             ylab = "Number of mortalities", main = "", col = "grey", 
             bor = "darkgrey", freq = TRUE, add = add, xaxt = 'n', ... )
    if (monthlabs) {
      axis(side = 1, at = seq(1/24, 23/24, length.out = 12), 
           labels = month.abb, tck = 0)
      axis(side = 1, at = (0:12)/12, labels = rep("", 13))
    } else {
      axis(side = 1, at = seq(0, 1, length.out = 6), 
           labels = seq(0, 1, length.out = 6))
    }
    
   }
  
  K <- ifelse(histogram, par("usr")[4]/ymax, 1)  # find the dimension of the plotting window
  
  if(!histogram && !add) {
    #need to make a new plot window!
    plot.new()
    xmax = par("xaxp")[2]
    plot.window(xlim = c(0, xmax), ylim = c(0, ymax))
    asp = par("xaxp")[3]
    if (monthlabs) {
      axis(side = 1, at = seq(1/24, 23/24, length.out = 12), 
           labels = month.abb, tck = 0)
      axis(side = 1, at = (0:12)/12, labels = rep("", 13))
    } else {
      axis(side = 1, at = seq(0, xmax, length.out = 6), 
           labels = seq(0, 1, length.out = 6))
    }
    mtext(ifelse(monthlabs, "Time of year", "Time within a period")
          , side = 1, at = xmax / 2, line = 2)
  }
  
  hazard.labs <- pretty(prediction$CI)
  if (!add) {
    # if we don't have a histogram our hazard axis should be on the left!
    if(yaxt != "n")  axis(ifelse(histogram, 4, 2), at = hazard.labs * K, 
                          hazard.labs, col = hazcolor)
    mtext(side = ifelse(histogram, 4, 2), line = par()$mgp[1], 
          "Estimated hazard function", las = 0, col = hazcolor)
    }
  
  if (plotCI) {
    with(prediction, {
      lines(t, fit*K, col = hazcolor, lwd = 2)
      t.poly <- c(t, t[length(t):1])
      CI.poly <- c(CI[1,], CI[2,length(t):1])*K
      polygon(t.poly, CI.poly, col = scales::alpha(hazcolor, alpha), bor = NA)
      lines(t, CI[1,]*K, col = hazcolor, lty = 3)
      lines(t, CI[2,]*K, col = hazcolor, lty = 3)
    })
  } else {
    with(prediction, {
      lines(t, fit*K, col = hazcolor, lwd = 2)
    })
  }
}