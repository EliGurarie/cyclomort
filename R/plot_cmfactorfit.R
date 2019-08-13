#' Plot cmfactorfit objects
#' 
#' @param x a cmfactorfit object
#' @param fit a character (either "null", "alt", or "both") that dictates what fits will be plotted
#' @param colors vector of colors (one component for each individual fit being plotted) for the hazard estimates
#' @param legend boolean parameter dictating whether or not a legend will be added to the plot
#' @param ... additional parameters to pass to the \code{\link{plot.cmfit}} function. Perhaps most usefully: lowering the default nreps (e.g. to 1000) makes plotting much faster. 
#' 
#' @return a plot comparing the hazard estimates from the null model with the individual estimates from each factor level
#' 
#' @example examples/predict_cmfit_example.R
#' @export

plot.cmfactorfit = function(x, fit = "both", colors = NULL, legend = TRUE, ...) {
  
  if (!fit %in% c("null", "alt", "both")) stop("Invalid \"fit\" parameter.")

  if (fit == "null" | fit == "both") {
    numPlots = 1
    nullFit = x$fits$null
    plot(nullFit, histogram = FALSE, yaxt = "n", ...)
  }
  
  if (fit == "alt" | fit == "both") {
    altFits = x$fits$alt
    numPlots = length(altFits)
    if(is.null(colors)) colors = hsv(h = 0:numPlots / numPlots, s = 1, v = 1)
    for (i in 1:numPlots) {
      add = fit == "both" | i > 1
      plot(altFits[[i]], hist = FALSE, add = add, hazcolor = colors[i], ...)
      #slight issue with axes (they only reflect the first model plotted).. any idea how to fix?
    }
  }
  
  axis(2)
  
  #is there a better place for me to put this?
  if(legend){
    par(xpd = TRUE)
    if (fit == "null") {
      legend("top", col = "black", legend = "Null model", 
             lty = 1, cex = .75, ncol = 1)
    } else if (fit == "alt") {
      legend("top", col = colors, 
             legend = paste("Alt", names(altFits), sep = ""), 
             lty = 1, cex = .75, ncol = numPlots)
    } else { # if fit == "both"
      legend("top", col = c("black", colors), 
             legend = c("Null", names(altFits)), 
             lty = 1, cex = .75, ncol = numPlots + 1)
    }
  }
}