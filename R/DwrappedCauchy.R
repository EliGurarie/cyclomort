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
#' plot(seq(-pi, pi, .01), dwrpCauchy(seq(-pi, pi, .01), 0, 0.7), type = "l")
#' @export 

dwrpCauchy <- function (theta, mu, rho) {
  (1 - rho^2)/((2 * pi) * (1 + rho^2 - 2 * rho * cos(theta - mu)));
}

#' CDF values for wrapped Cauchy distribution
#'
#' Derive an integral to obtain the CDF value of wrapped Cauchy distribution at a point
#' 
#' @param q desired value of the point, ranging from -pi to pi
#' @param mu mean or peak parameter
#' @param rho concentration parameter (between -1 and 1), equivalent to the E(cos(theta))
#' 
#' @return CDF value for the given point, ranging from 0 to 1
#' 
#' @examples
#' plot(Vectorize(pwrpCauchy, vectorization.args = "p")(seq(-pi, pi, 0.01), 0, 0.7), type = "l")
#' @export

pwrpCauchy = function(q, mu, rho) {
  integrate(dwrpCauchy, -pi, q, mu = mu, rho = rho)$value
}

#' CDF quantiles for wrapped Cauchy distribution
#'
#' Obtains quantile values for the wrapped Cauchy CDF
#'
#' @param p desired quantile (ranging from 0 to 1)
#' @param mu mean or peak parameter
#' @param rho concentration parameter (between -1 and 1), equivalent to the E(cos(theta))
#' 
#' @return quantile value for the distribution on the scale of -pi to pi
#' @examples
#' plot(Vectorize(qwrpCauchy, vectorization.args = "p")(seq(0, 1, 0.01), 0, 0.7), type = "l")
#' @export

qwrpCauchy = function(p, mu, rho, tol = 1e-5) {
  #simple bisection for value
  a = -pi
  b = pi
  mid = 0.5 * (a+b)
  value = pwrpCauchy(mid, mu, rho)
  while (abs(value - p) > tol) {
    if (value > p) {
      b = mid
      mid = 0.5 * (a+b)
    } else {
      a = mid
      mid = 0.5 * (a+b)
    }
    value = pwrpCauchy(mid, mu, rho)
  }
  mid
}