T.morts1 <- simPeriodicMorts(300, period = 1, 
                             peaks = c(0.25, 0.75), 
                             rhos = c(0.8, 0.5), 
                             weights = 0.3, dt = .01, 
                             A = .02, plotme = FALSE)

p0 = c(A = 0.05, peak1 = 0.25, peak2 = 0.75, rho1 = 0.5, rho2 = 0.5, weight1 = 0.5)
##MLE for parameters based on simulated data
fits = fit_cyclomort(T.morts1, p0, dt = .01, period = 1)
getCIs(fits)
##Actual parameter values from simulated data
attributes(T.morts1)