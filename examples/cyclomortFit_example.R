# Simulate data
T.morts1 <- simulate_cycloSurv(1000, period = 365, 
                             meanhazard = 0.3 / 365, 
                             peaks = c(0.25 * 365, 0.75 * 365), 
                             durations = c(0.3 * 365, 0.1 * 365), 
                             weights = c(0.7,0.3), 
                             plotme = FALSE)

# Estimate simulated data
fits <- fit_cyclomort(T.morts1, n.seasons = 2)
fits

# Plot results
plot(fits, nreps = 1000, monthlabs = TRUE)
# NB: `nreps` is for the bootstrap of the confidence interval 
# The default (5000) is slower but smoother

# Actual parameter values from simulated data
attributes(T.morts1)
