## ----setup, include = FALSE----------------------------------------------
require(cyclomort)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----plotWC, fig.width = 7, fig.align = "center", echo = FALSE-----------
require(cyclomort)

curve(wc(x, mu = 100, rho = .7, tau = 365), xlim = c(0,365), n = 1e4, ylab = "hazard", xlab = "time")
curve(wc(x, mu = 100, rho = .5, tau = 365), add = TRUE, col = 2)
curve(wc(x, mu = 100, rho = .3, tau = 365), add = TRUE, col = 3)
legend("topright", col = 1:3, legend = c(.7,.5,.3), title = "rho", lty = 1)

## ----plotMWC, fig.width = 7, fig.align = "center", echo = FALSE----------
curve(mwc(x, mus = c(0.125, 0.5), rhos = c(0.7, 0.5), gammas = c(2, 1), tau = 1), xlim = c(0,1), ylab = "hazard", xlab = "time")
curve(mwc(x, mus = c(0.25, 0.75), rhos = c(0.3, 0.8), gammas = c(0.6, 0.4), tau = 1), add = TRUE, col = 2)
curve(mwc(x, mus = c(0.25, 0.5, 0.75), rhos = c(0.6, 0.5, 0.4), gammas = c(0.5, 0.2, 0.3), tau = 1), add = TRUE, col = 3)

## ----plotSim, fig.width = 7, fig.height = 5, fig.align = "center", echo = -1----
set.seed(10)
T.morts.sim <- simulate_cycloSurv(300, period = 365, 
                             meanhazard = 1/365, 
                             peaks = c(100, 250), 
                             durations = c(25, 40), 
                             weights = c(0.4, 0.6), 
                             plotme = TRUE,
                             max.periods = 5)

## ------------------------------------------------------------------------
T.morts.sim[1:10]
class(T.morts.sim)

## ------------------------------------------------------------------------
T.morts.fit <- fit_cyclomort(T.morts.sim, n.seasons = 2)

## ------------------------------------------------------------------------
print(T.morts.fit)

## ------------------------------------------------------------------------
summary(T.morts.fit)

## ----plotSimFits, fig.width = 7, fig.height = 5, fig.align = "center"----
plot(T.morts.fit, breaks = 30)

## ---- message = FALSE----------------------------------------------------
T.morts.1season.fit <- fit_cyclomort(T.morts.sim, n.seasons = 1)
T.morts.null.fit <- fit_cyclomort(T.morts.sim, n.seasons = 0)
plot(T.morts.fit, histogram = FALSE, monthlabs = TRUE)
plot(T.morts.1season.fit, histogram = FALSE, add = TRUE, hazcolor = "red")
plot(T.morts.null.fit, histogram = FALSE, add = TRUE, hazcolor = "blue")

## ---- eval = 2, echo = 1-------------------------------------------------
timetoeventprediction <- predict(T.morts.fit, t = 1:365, type = "timetoevent", CI = TRUE, nreps = 100)
data(timetoeventprediction)

## ------------------------------------------------------------------------
with(timetoeventprediction, {
  plot(t, fit, type = "l", ylab = "Time to event", ylim = range(CI), lwd = 2)
  lines(t, CI[1,])
  lines(t, CI[2,])
})

## ------------------------------------------------------------------------
T.select <- select_seasons(T.morts.sim, max.season = 3)

## ------------------------------------------------------------------------
data(seasonalsex)
str(seasonalsex)

## ------------------------------------------------------------------------
Survival.factorfit <- factorfit_cyclomort(event ~ sex, data = seasonalsex, n.seasons = 1)
summary(Survival.factorfit)

## ------------------------------------------------------------------------
plot(Survival.factorfit, ymax = 1.2)

