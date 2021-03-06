# simulate two-peak mortality process
sim.morts <- simulate_cycloSurv(300, period = 1, peaks = c(0.3, 0.8), 
                      durations = c(0.15, 0.20), weights = c(3, 2)/5, 
                      meanhazard = 1, plotme = FALSE, max.periods = 6)
sim.morts <- simulate_cycloSurv(300, period = 365, peaks = c(0.3, 0.8)*365, 
                                durations = c(0.15, 0.20)*365, weights = c(3, 2)/5, 
                                meanhazard = 1/365, plotme = FALSE, max.periods = 6)

# estimate parameters
sim.morts.fit <- fit_cyclomort(sim.morts, n.seasons = 2)

# compute predictions for one moment in time (with 95% confidence interval)
predict(sim.morts.fit, CI = TRUE, type = "hazard")

# compute predictions for a range of times
predict(sim.morts.fit, t = 1:365, CI = FALSE, type = "hazard")

# these predictions are used (internally) in the plot.cmfit method:

\donttest{
plot(sim.morts.fit, CI.level = 0.95, months = FALSE, histogram = FALSE, monthlabs = TRUE)
plot(sim.morts.fit, CI.level = 0.8, months = FALSE, histogram = FALSE, add = TRUE)
plot(sim.morts.fit, CI.level = 0.5, months = FALSE, histogram = FALSE, add = TRUE)
}

# predict time to event given a start at times (this is a very slow calculation!)

\donttest{
timetoeventprediction <- predict(sim.morts.fit, t = seq(1,365,3), type = "timetoevent",
                       CI = TRUE, nreps = 1e2)
}

# the following object contains a prediction
data(timetoeventprediction)

with(timetoeventprediction, {
  plot(t, fit, type = "l", lwd = 2,  main = "expected time to event", 
       ylim = c(100,365), ylab = "days")
  lines(t, CI[1,], lty = 3)
  lines(t, CI[2,], lty = 3)
})
