#' Plot cmfactorfit objects
#' 
#' @param x a cmfactorfit object
#' @param fit a character (either "null", "alt", or "both") that dictates what fits will be plotted
#' 
#' @return a plot comparing the estimates from the null model with the individual estimates from each factor level
#' 
#' @example examples/predict_cmfit_example.R
#' @export

plot.cmfactorfit = function(x, fit = "both") {
  
  if (!fit %in% c("null", "alt", "both")) stop("Invalid \"fit\" parameter.")
  
  if (fit == "null" | fit == "both") {
    numPlots = 1
    nullFit = x$fits$null
    plot(nullFit, hist = FALSE)
  }
  
  if (fit == "alt" | fit == "both") {
    altFits = x$fits$alt
    numPlots = length(altFits)
    colors = hsv(h = 0:numPlots / numPlots, s = 1, v = 1)
    for (i in 1:numPlots) {
      add = fit == "both" | i > 1
      plot(altFits[[i]], hist = FALSE, add = add, hazcolor = colors[i])
      #slight issue with axes (they only reflect the first model plotted).. any idea how to fix?
    }
  }
  
  #is there a better place for me to put this?
  par(xpd = TRUE)
  yVal = par("usr")[4] * 1.25
  if (fit == "null") {
    legend(x = 0, y = yVal, col = "black", legend = "Null model", 
           lty = 1, cex = .75, ncol = 1)
  } else if (fit == "alt") {
    legend(x = 0, y = yVal, col = colors, 
           legend = paste("Alt", 1:numPlots, sep = ""), 
           lty = 1, cex = .75, ncol = numPlots)
  } else { # if fit == "both"
    legend(x = 0, y = yVal, col = c("black", colors), 
           legend = c("Null model", paste("Alt", 1:numPlots, sep = "")), 
           lty = 1, cex = .75, ncol = numPlots + 1)
  }
  
}