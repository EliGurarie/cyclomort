#' Wrapped Cauchy distribution
#' 
#' Very basic Wrapped Cauchy distribution function.
#' 
#' @param theta angle 
#' @param mu mean parameter
#' @param rho concentration parameter (between -1 and 1), equivalent to the E(cos(theta))
#' 
#' @return  the value of the p.d.f. of the distribution
#' @examples
#' plot(DwrappedCauchy(seq(-pi, pi, .01), 0, 0.7), type = "l")
#' @export 

DwrappedCauchy <- function (theta, mu, rho) {
  (1 - rho^2)/((2 * pi) * (1 + rho^2 - 2 * rho * cos(theta - mu)));
}
