T <- simulate_cycloMort(300, period = 1, peaks = c(0.3, 0.8), 
                      durations = c(0.15, 0.20), weights = c(3, 2)/5, 
                      meanhazard = 1, plotme = FALSE, max.periods = 6)
T.fit <- fit_cyclomort(T, n.seasons = 2)
predictions = predict(T.fit, t = 0.5, CI = TRUE, type = "hazard")

plot(T.fit)