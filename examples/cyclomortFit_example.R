T.morts1 <- simulate_cycloSurv(1000, period = 365, 
                             meanhazard = 0.3 / 365, 
                             peaks = c(0.25 * 365, 0.75 * 365), 
                             durations = c(0.3 * 365, 0.1 * 365), 
                             weights = c(0.7,0.3), 
                             plotme = FALSE)

##MLE for parameters based on simulated data
fits = fit_cyclomort(T.morts1, n.seasons = 2)
print(fits)
plot(fits)

##Actual parameter values from simulated data
attributes(T.morts1)
