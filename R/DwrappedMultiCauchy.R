DwrappedMultiCauchy <-
function(x, A, mus, rhos, weights) {
  weights = c(weights, 1 - sum(weights));
  f = function(t) {
    sum(DwrappedCauchy(t, mus, rhos) * weights);
  }
  A * Vectorize(f)(x);
}
