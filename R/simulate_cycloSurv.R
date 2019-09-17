#' Simulate periodic mortality process
#' 
#' @param n number of simulated mortality/censoring events
#' @param meanhazard average hazard value
#' @param peaks k-vector of peaks
#' @param durations k-vector of season length parameters, based on concentration parameter from wrapped Cauchy distribution
#' @param weights k-vector of weights ((k-1)-vector is also accepted)
#' @param censoring the type of censoring in the simulated data. Either "none" (all data is uncensored), "fixed" (all data is censored at a specified time), or "random" (data is randomly censored throughout).
#' @param censor.times numeric or vector listing times for censoring (only applicable if censoring == "fixed").
#' @param period length of one mortality cycle
#' @param n.times number of x-values for plots (a higher value results in more precision for curves)
#' @param max.periods maximum number of cycles
#' @param plotme if TRUE, produces a set of plots for the simulation to display its accuracy
#' 
#' @return  a \code{cycloSurv} object (a type of Surv object; see \code{\link{Surv}})
#' 
#' @example examples/simulate_cycloSurv_example.R
#' @export

simulate_cycloSurv <- function(n, period = 1, 
         meanhazard = 0.5, 
         peaks = c(0.25, 0.75), 
         durations = c(0.2, 0.1), 
         weights = c(0.5, 0.5), 
         censoring = "random",
         censor.times = max.periods * period / 2,
         max.periods = 10, 
         n.times = 1e3, 
         plotme = TRUE) {
  
  if (!censoring %in% c("none", "fixed", "random")) 
    stop("Invalid censoring format.")
  
  censor.times = rep(censor.times, n)
  
  if(length(weights) == length(peaks) - 1){
    message("Filling out the weight vector.")
    weights <- c(weights, 1 - sum(weights))
  }
  
  if(sum(weights) != 1){
    message("Weights do not sum to 1 ... we kindly fixed that for you.")
    weights <- weights/sum(weights)
  }
  
  gammas = meanhazard * weights

  rhos <- findRho(durations/period)
  t <- seq(0, max.periods*period, length = n.times)
  
  hazard = mwc(t, peaks, rhos, gammas, period)
  cumhazard = imwc(t, peaks, rhos, gammas, period)
  
  cum.prob.survival <-  exp(-cumhazard)
  cum.mortality <- 1 - cum.prob.survival
  pdf.mortality <- hazard * cum.prob.survival
  
  sampleFromPdf <- function(n, pdf){
    pdf[,2] <- pdf[,2]/max(pdf[,2])
    xlim <- range(pdf[,1])
    XY.scatter <- cbind(sample(pdf[,1], n*1e2, replace = TRUE), runif(n*1e2))
    sample <- apply(XY.scatter, 1, function(v) if(v[2] < pdf[pdf[,1] == v[1],2]) 
      return(v[1])) 
    sample <- unlist(sample)
    return(sample[1:n])
  }
  
  rawTimes = sampleFromPdf(n, cbind(t, pdf.mortality))
  #times if no censoring existed
  if (censoring == "random") censor.times = runif(n, min = 0, 
                                                  max = max.periods * period)
  if (censoring == "none") censor.times = rep(max.periods * period + 1, n)
  
  morts_t <- rawTimes
  censored = (morts_t > censor.times)
  morts_t[morts_t <= 0] = 1e-6
  morts_t[censored] = censor.times[censored]
  morts = Surv(time = rep(0, length(morts_t)), 
               time2 = morts_t, event = !censored)
  
  attributes(morts)$meanhazard <- meanhazard
  attributes(morts)$peaks <- peaks
  attributes(morts)$durations <- durations
  attributes(morts)$weights <- weights
  attributes(morts)$period <- period
  attributes(morts)$phase <- 0
  
  if(plotme){
    par.init <- par(no.readonly = TRUE)
    on.exit(par(par.init))
    par(mfrow = c(2,2), bty = "l", mar = c(2,4,4,2), tck = 0.02, 
        mgp = c(1.5,.25,0), xpd = NA)
    plot(t, hazard, type = "l", main = "hazard function", xlab = "time")
    plot(t, cum.prob.survival, type = "l", ylim = c(0,1), 
         main = "survival curve", xlab = "time")
    plot(t, cum.mortality, type = "l", ylim = c(0,1.1), 
         main = "cumulative mortality: F(t)", xlab = "time")
    abline(h = 1, col = "grey", lty = 3, xpd = FALSE, lwd = 2)
    hist(rawTimes, breaks = seq(0, max.periods*period, period / 6), 
         col = "grey", bor = "darkgrey", freq = FALSE, 
         main = "mortality distribution", xlab = "time", ylab = "density")
    lines(t, pdf.mortality, type = "l")
    legend("topright", lty = c(NA, 1), pch = c(22, NA),
           pt.bg = c("grey", NA), bty = "n", 
           col = c("darkgrey", "black"), 
           pt.cex = 2, 
           legend = c("simulated", "p.d.f."))
    suppressWarnings(par(par.init))
  }
  
  class(morts) = c("cycloSurv", "Surv")
  
  return(morts)
}
