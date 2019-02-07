#' Mixed Cauchy distribution
#' 
#' Very basic Wrapped Cauchy shaped function
#' 
#' @param theta angle 
#' @param mus k-vector of mean parameters
#' @param rhos k-vector of concentration parameters
#' @param weights (k-1)-vector of weights - the sum must be less than 1
#' @param A overall magnitude
#' @seealso DwrappedCauchy
#' 
#' @value the value of the p.d.f. of the mixed distribution
#' @examples
#' plot(DwrappedMultiCauchy(seq(-pi, pi, .01), 
#' A = 1, 
#' mus = c(-1,1), 
#' rhos = c(0.7,.5), 
#' weights = 0.7), type = "l")
#' @export 
#' 
DwrappedMultiCauchy <-
function(theta, mus, rhos, weights, A=1) {
  weights = c(weights, 1 - sum(weights));
  f = function(t) {
    sum(DwrappedCauchy(t, mus, rhos) * weights);
  }
  A * Vectorize(f)(theta);
}
