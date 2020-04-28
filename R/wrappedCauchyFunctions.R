#' Wrapped Cauchy and Integrated Wrapped Cauchy functions 
#' 
#' Fundamental periodic hazard function and its (analytical) integral - required 
#' for efficient estimation. These are mainly internal functions. They are both 
#' parameterized in terms of peak mean  \eqn{\mu}, concentration parameter  
#' \eqn{\rho}, and period \eqn{\tau}. They are "unweighted", i.e. 
#' \deqn{\int_0^\tau f(t) dt = \tau}
#' 
#' @param t  time
#' @param mu mean peak
#' @param rho concentration parameter (0 <= rho <= 1)
#' @param tau period
#' @example demo/iwc_example.R
#' @export 
#' @aliases iwc

wc <- function(t, mu, rho, tau)
  (1 - rho^2) / (1 + rho^2 - 2*rho*cos((t - mu) * 2 * pi/tau))

#' @rdname wc
#' @export 
iwc <- function(t, mu, rho, tau){
  mu.star <-  - mu * 2 * pi / tau
  t.star <- (t - mu) * 2 * pi / tau
  -(tau / pi) *  (
    atan((rho + 1) * sin(t.star)/((rho - 1) * (cos(t.star) + 1) )) - 
      atan((rho + 1) * sin(mu.star)/((rho - 1) * (cos(mu.star) + 1))) +
      -pi * (ceiling((t - mu)/tau - 1/2) + ifelse(mu < tau/2, 0, 1)))
}

#' Mixed Wrapped Cauchy and Integrated Mixed Wrapped Cauchy Functions
#' 
#' Allows for the fitting of models with multiple "seasons" of high mortality by 
#' producing a multimodal version of the wrapped Cauchy function. A simple 
#' mixture model that evaluates the wrapped Cauchy function for each individual 
#' component and then combines all components according to the individual linear 
#' weights.
#' @param mus k-vector of mean peaks (assuming k seasons)
#' @param rhos k-vector of concentration parameters
#' @param gammas k-vector of average hazard values for each component
#' @rdname wc
#' @export 
mwc <- function(t, mus, rhos, gammas, tau){
  rowSums(Vectorize(wc, vectorize.args = c("mu", "rho"))(t = t, mu = mus, rho = 
                                                           rhos, tau = tau) %*% 
            gammas)
}

#' @rdname wc
#' @export 
imwc <- function(t, mus, rhos, gammas, tau){
  rowSums(Vectorize(iwc, vectorize.args = c("mu", "rho"))(t = t, mu = mus, rho = 
                                                            rhos, tau = tau) %*% 
            gammas)
}


