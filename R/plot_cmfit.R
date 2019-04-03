#' Plot cmfit objects
#' 
#' @param x a cmfit object
#' 
#' @return a plot comparing the estimated mortality curve (based on parameter estimates)
#' and the actual results (as a histogram).
#' 
#' @example examples/cyclomortFit_example.R
#' @export

plot.cmfit = function(x, n.times = 1e3) {
  uncensoredData = as.numeric(x$data[x$data[,2] == 1,1])##uncensored data
  h = hist(uncensoredData, xlab = "Number of periods", ylab = "Number of mortalities",
       main = "Comparing parameter estimates with actual mortality data", breaks = 40)
  
  t <- seq(0, max(uncensoredData), 1/n.times)
  
  rawEstimates = fits$rawpars[,1]
  
  mus = rawEstimates[grepl("mu", names(rawEstimates))]
  rhos = rawEstimates[grepl("rho", names(rawEstimates))]
  gammas = rawEstimates[grepl("gamma", names(rawEstimates))]
  period = x$period
  
  hazard = mwc(t, mus, rhos, gammas, period)
  cumhazard = imwc(t, mus, rhos, gammas, period)
  cum.prob.survival <-  exp(-cumhazard)
  cum.mortality <- 1 - cum.prob.survival
  pdf.mortality <- hazard * cum.prob.survival
  adj.pdf.mortality = pdf.mortality / max(pdf.mortality) * max(h$counts)
  
  lines((1:length(pdf.mortality)) * (1/n.times), adj.pdf.mortality)
}
