
# distribution function
curve(wc(x, mu = 100, rho = .7, tau = 365), xlim = c(0,365), n = 1e4, 
      ylab = "hazard", xlab = "time")
curve(wc(x, mu = 100, rho = .5, tau = 365), add = TRUE, col = 2)
curve(wc(x, mu = 100, rho = .3, tau = 365), add = TRUE, col = 3)

#graphs of mixed wrapped Cauchy functions
curve(mwc(x, mus = c(0.125, 0.5), rhos = c(0.7, 0.5), 
          gammas = c(2, 1), tau = 1), xlim = c(0,1), ylab = "hazard", xlab = "time")
curve(mwc(x, mus = c(0.25, 0.75), rhos = c(0.3, 0.8), 
          gammas = c(0.6, 0.4), tau = 1), add = TRUE, col = 2)
curve(mwc(x, mus = c(0.25, 0.5, 0.75), rhos = c(0.6, 0.5, 0.4), 
          gammas = c(0.5, 0.2, 0.3), tau = 1), add = TRUE, col = 3)
