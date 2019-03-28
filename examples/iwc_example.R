
# distribution function
curve(dwc(x, mu = 100, rho = .7, tau = 365), xlim = c(0,365), n = 1e4, ylab = "hazard", xlab = "time")
curve(dwc(x, mu = 100, rho = .5, tau = 365), add = TRUE, col = 2)
curve(dwc(x, mu = 100, rho = .3, tau = 365), add = TRUE, col = 3)


# compare numerical and analytical integral

require(microbenchmark)
microbenchmark(
  I1 <- integrate(dwc, mu =  100, rho = .1, tau = 365,  lower = 0, upper = 1000)$value, 
  I2 <- iwc(t = 1000, mu =  100, rho = .1, tau = 365)
)

I1
I2
