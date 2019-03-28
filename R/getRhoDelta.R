#' Convert Rho to Tau
#' 
#' Functions for converting the concentration parameter \eq{\rho} to the season duration parameter \eq{\delta} and vice versa.  They are: \code{findDelta(rho)} and \code{findDelta(delta)}.  These are not very exciting functions, but making them work correctly involved quite a bit of calculus and algebra!
#' 
#' 
#' @examples 
#' @seealso iwc
#' findDelta(rho = 0.9); findRho(0.0167)
#' findDelta(rho = 0.1); findRho(0.218)
#' rhos <- seq(0, 1, length = 1e3)
#' 
#' # Plot the relationship
#' par(mfrow = c(1,2))
#' plot(rhos, Vectorize(findDelta, vectorize.args = "rho")(rhos), ylab = "deltas", type = "l")
#' deltas <- seq(0, .25, length = 1e3)
#' plot(deltas, Vectorize(findRho, vectorize.args = "delta")(deltas), ylab = "rhos", type = "l")

#' @export
findDelta <- function(rho){
  ifelse(rho == 0, 0.25, 
         ifelse(rho == 1, 0, 
                uniroot(DeltaToRho, rho = rho, interval = c(0, .5))$root))
}

#' @export
findRho <- function(delta){
  RhoToDelta <- function(rho, delta) DeltaToRho(delta, rho)
  ifelse(delta == 0.25, 0, 
         ifelse(delta == 0, 1,
                uniroot(RhoToDelta, delta = delta, interval = c(0+1e-6, 1-1e-6))$root))
}

#' @export
DeltaToRho <- function(delta, rho){
  iwc(t = .5 + delta, mu = .5, rho = rho, tau = 1) - 
    iwc(t = .5 - delta, mu = .5, rho = rho, tau = 1) - 1/2
}
