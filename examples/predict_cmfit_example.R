T <- simPeriodicMorts(300, period = 1, peaks = c(0.3, 0.8), 
                      durations = c(0.15, 0.20), weights = c(3, 2)/5, 
                      meanhazard = 1, plotme = FALSE, max.periods = 6)
T.fit <- fit_cyclomort(T, n.seasons = 2)
predictions = predict(T.fit, t = 0.5, CI = TRUE, type = "timetodeath")

# The plotting method uses this function.
plot(T.fit, CI = TRUE, nreps = 1e3, breaks = 40)