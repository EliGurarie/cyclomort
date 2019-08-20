## ----setup, include = FALSE----------------------------------------------
require(cyclomort)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.width = 7, fig.align = "center", echo = FALSE------------------
curve(wc(x, mu = 100, rho = .7, tau = 365), xlim = c(0,365), n = 1e4, ylab = "hazard", xlab = "time")
curve(wc(x, mu = 100, rho = .5, tau = 365), add = TRUE, col = 2)
curve(wc(x, mu = 100, rho = .3, tau = 365), add = TRUE, col = 3)

## ---- fig.width = 7, fig.align = "center", echo = FALSE------------------
curve(mwc(x, mus = c(0.125, 0.5), rhos = c(0.7, 0.5), gammas = c(2, 1), tau = 1), xlim = c(0,1), ylab = "hazard", xlab = "time")
curve(mwc(x, mus = c(0.25, 0.75), rhos = c(0.3, 0.8), gammas = c(0.6, 0.4), tau = 1), add = TRUE, col = 2)
curve(mwc(x, mus = c(0.25, 0.5, 0.75), rhos = c(0.6, 0.5, 0.4), gammas = c(0.5, 0.2, 0.3), tau = 1), add = TRUE, col = 3)

## ---- fig.width = 7, fig.height = 5, fig.align = "center"----------------
T.morts1 <- simulate_cycloSurv(300, period = 365, 
                             meanhazard = 0.5/365, 
                             peaks = c(100, 250), 
                             durations = c(20, 40), 
                             weights = 0.4, 
                             plotme = TRUE,
                             max.periods = 5)

## ------------------------------------------------------------------------
T.morts1 <- simulate_cycloSurv(300, period = 365, 
                             meanhazard = 0.5/365, 
                             peaks = c(100, 250), 
                             durations = c(20, 40), 
                             weights = 0.4, 
                             plotme = FALSE,
                             max.periods = 5)

##MLE for parameters based on simulated data
fits = fit_cyclomort(T.morts1)
print(fits)
plot(fits)
##Actual parameter values from simulated data
attributes(T.morts1)

## ------------------------------------------------------------------------
T.morts1 <- simulate_cycloSurv(300, period = 365, 
                             meanhazard = 0.5/365, 
                             peaks = c(100, 250), 
                             durations = c(20, 40), 
                             weights = 0.4, 
                             plotme = FALSE,
                             max.periods = 5)
#a two-season simulated dataset

fits = select_seasons(T.morts1, max.season = 4)


## ------------------------------------------------------------------------
data(seasonalsex)
x <- factorfit_cyclomort(event ~ sex, data = seasonalsex, n.seasons = 1)

summary(x, coefs = TRUE)
plot(x)

