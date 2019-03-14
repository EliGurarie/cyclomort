#' Plot cmfit objects
#' 
#' @param x a cmfit object
#' 
#' @return a plot comparing the estimated mortality curve (based on parameter estimates)
#' and the actual results (as a histogram).
#' 
#' @example examples/cyclomortFit_example.R
#' @export

plot.cmfit = function(x) {
  uncensoredData = as.numeric(x$data[x$data[,2] == 1,1])##uncensored data
  h = hist(uncensoredData, xlab = "Number of periods", ylab = "Number of mortalities",
       main = "Comparing parameter estimates with actual mortality data", breaks = 40)
  
  
  getHazard = function(t, fit) {
    A = fit$A[1]
    mus = sapply(fit[grepl("peak", names(fit))], head, 1)
    rhos = sapply(fit[as.logical(grepl("rho", names(fit)) * !grepl("season", names(fit)))], head, 1)
    weights = sapply(fit[grepl("weight", names(fit))], head, 1)
    
    period = fit$period
    tt = t/period*2*pi - pi
    pks = mus/period*2*pi - pi
    dwrpMultiCauchy(tt, mus = pks, rhos = rhos, weights = weights, A = A) / fit$dt
  }
  
  t <- seq(0, max(uncensoredData), x$dt)
  
  hazard = getHazard(t)
  cum.prob.survival = cumprod(1-hazard*x$dt)
  cum.mortality = 1 - cum.prob.survival
  pdf.mortality = c(0,diff(cum.mortality)*x$dt)
  adj.pdf.mortality = pdf.mortality / max(pdf.mortality) * max(h$counts)
  
  lines((1:length(pdf.mortality)) * x$dt, adj.pdf.mortality)
}
