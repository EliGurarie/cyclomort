#' Convert Rho to Tau
#' 
#' Functions for converting the concentration parameter rho to the season duration parameter delta and vice versa.  They are: \code{findDelta(rho)} and \code{findDelta(delta)}.  These are not very exciting functions, but making them work correctly involved quite a bit of calculus and algebra!
#' 
#' 
#' @examples 
#' findDelta(rho = 0.9); findRho(0.0167)
#' findDelta(rho = 0.1); findRho(0.218)
#' 
#' 
#' # Plot the relationship
#' par(mfrow = c(1,2))
#' rhos <- seq(0, 1, length = 1e3)
#' plot(rhos, findDelta(rhos), ylab = "deltas", type = "l")
#' deltas <- seq(0, .5, length = 1e3)
#' plot(deltas, findRho(deltas), ylab = "rhos", type = "l")

#' @export
findDelta <- Vectorize(function(rho){
  ifelse(rho == 0, 0.5, 
         ifelse(rho == 1, 0, 
                uniroot(DeltaToRho, rho = rho, interval = c(0, .5))$root))
})

#' @rdname findDelta
#' @export
findRho <- Vectorize(function(delta){
  RhoToDelta <- function(rho, delta) DeltaToRho(delta, rho)
  ifelse(delta == 0.5, 0, 
         ifelse(delta == 0, 1,
                uniroot(RhoToDelta, delta = delta, interval = c(1e-6, 1-1e-6))$root))
})

#' @rdname DeltaToRho
#' @param delta seasonal duration parameter
#' @param rho concentration parameter ([0, 1])
#' @export
DeltaToRho <- function(delta, rho){
  iwc(t = .5 + delta/2, mu = .5, rho = rho, tau = 1) - 
    iwc(t = .5 - delta/2, mu = .5, rho = rho, tau = 1) - 1/2
}
