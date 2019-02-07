#' Simulate periodic mortality process
#' 
#' @example examples/simPeriodicMort_example.R
#' @export

simPeriodicMorts <-
function(n, A = 0.01, peaks = c(0.25, 0.75), rhos = c(0.6, 0.6), weights = c(0.5), fixedCensoring = FALSE,
                            period = 1, dt = 0.01, max.periods = 10, plotme = TRUE) {
  #Simulates mortalities using a right-censored method with a random censoring time
  
  t = seq(0, max.periods, dt);
  
  #Hazard function could theoretically vary, but parameters required for simPeriodicMorts would change
  getHazard = function(t, p, r, w, period, dt) {
    tt = t / period * 2 * pi;
    mus = p / period * 2 * pi;
    DwrappedMultiCauchy(tt, A, mus, r, w) / dt;
  }
  
  hazard = getHazard(t, peaks, rhos, weights, period, dt);
  
  cum.prob.survival <- cumprod(1-hazard*dt);
  cum.mortality <- 1 - cum.prob.survival;
  pdf.mortality <- c(0,diff(cum.mortality)*dt);
  
  if(plotme){
    par(mfrow = c(2,2), bty = "l", mar = c(2,4,4,2), tck = 0.02, mgp = c(1.5,.25,0), xpd = NA);
    plot(t, hazard, type = "l", main = "hazard function");
    plot(t, cum.prob.survival, type = "l", ylim = c(0,1), main = "survival curve");
    plot(t, cum.mortality, type = "l", ylim = c(0,1), main = "cumulative mortality: F(t)");
    plot(t, pdf.mortality, type = "l", main = "pdf of mortality: f(t)");
  }
  
  pdf <- cbind(t, pdf.mortality);
  
  sampleFromPdf <- function(n, pdf){
    pdf[,2] <- pdf[,2]/max(pdf[,2]);
    xlim <- range(pdf[,1]);
    XY.scatter <- cbind(sample(pdf[,1], 1e4, replace = TRUE), runif(1e4));
    sample <- apply(XY.scatter, 1, function(v) if(v[2] < pdf[pdf[,1] == v[1],2]) return(v[1])); 
    sample <- unlist(sample);
    return(sample[1:n]);
  }
  
  morts_u = sampleFromPdf(n, pdf);
  if (fixedCensoring) {
    morts_d = rep(norm(1, max.periods*period/2, period), length(morts_u));
  } else {
    morts_d = rnorm(n, max.periods*period/2, period);
  }
  censored = (morts_u > morts_d);
  morts_u[censored] = morts_d[censored];
  morts_d[censored] = 0;
  morts_d[!censored] = 1;
  #morts_u are the times of death or the times of censoring
  #morts_d is an indicator function for whether the data is censored or not (1 if not censored)
  morts = matrix(c(morts_u, morts_d), ncol = 2);
  
  attributes(morts)$A <- A;
  attributes(morts)$peaks <- peaks;
  attributes(morts)$rhos <- rhos;
  attributes(morts)$weights <- weights;
  
  return(morts);
}
