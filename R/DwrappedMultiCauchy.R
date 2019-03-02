#' Mixed Cauchy distribution
#' 
#' Very basic Wrapped Cauchy shaped function. Incorporates any number of mixed
#' distributions in order to simulate a potential function with a variety of differently
#' shaped peaks over a finite period.
#' 
#' @param theta angle 
#' @param mus k-vector of mean parameters
#' @param rhos k-vector of concentration parameters
#' @param weights (k-1)-vector of weights - the sum must be less than 1
#' @param A overall magnitude
#' @seealso DwrappedCauchy
#' 
#' @return the value of the p.d.f. of the mixed distribution
#' @examples
#' plot(seq(-pi, pi, .01), 
#' dwrpMultiCauchy(seq(-pi, pi, .01), 
#' A = 1, 
#' mus = c(-1,1), 
#' rhos = c(0.7,0.5), 
#' weights = 0.7), type = "l", ylab = "Probability Value")
#' @export 
#' 
dwrpMultiCauchy <-
function(theta, mus, rhos, weights, A=1) {
  weights = c(weights, 1 - sum(weights))
  A * rowSums(Vectorize(dwrpCauchy, vectorize.args = c("mu", "rho"))(theta = theta, mu = mus, rho = rhos) %*% weights)
}
