## ----setup, include = FALSE---------------------------------------------------
require(cyclomort)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ">",
  fig.width = 6,
  fig.height = 4
)

## -----------------------------------------------------------------------------
library(cyclomort)

## ----plotWC, fig.align = "center", echo = FALSE, echo = 2:5-------------------
oldpar <- par(no.readonly =TRUE); par(bty = "l", mar = c(4,4,2,2))
curve(wc(x, mu = 100, rho = .7, tau = 365), xlim = c(0,365), n = 1e4, ylab = "hazard", xlab = "time")
curve(wc(x, mu = 100, rho = .5, tau = 365), add = TRUE, col = 2)
curve(wc(x, mu = 100, rho = .3, tau = 365), add = TRUE, col = 3)
legend("topright", col = 1:3, legend = c(.7,.5,.3), title = "rho", lty = 1)
par(oldpar)

## ----plotMWC, fig.align = "center", echo = 2:7--------------------------------
oldpar <- par(no.readonly =TRUE); par(bty = "l", mar = c(4,4,2,2), xpd = NA)
mus <-  c(.3,.5,.9)
rhos <-  c(.5,.9,.7)
gammas <- c(.6,.1,.3)
curve(mwc(x, mus = mus[1], rhos = rhos[1], gammas = 1, tau = 1), 
      xlim = c(0,1), ylab = "hazard", xlab = "time")
curve(mwc(x, mus = mus[1:2], rhos = rhos[1:2], gammas = gammas[1:2]/sum(gammas[1:2]), tau = 1), 
      add = TRUE, col = 2)
curve(mwc(x, mus = mus, rhos = rhos, gammas = gammas, tau = 1), 
      add = TRUE, col = 3)
par(oldpar)

## ----plotSim, fig.align = "center", echo = -1---------------------------------
set.seed(1976)
T.morts.sim <- simulate_cycloSurv(n = 300, period = 365, meanhazard = 1/365, 
                             peaks = c(100, 250), durations = c(25, 40), 
                             weights = c(0.6, 0.4), plotme = TRUE,
                             max.periods = 5)

## -----------------------------------------------------------------------------
T.morts.sim[1:10]
class(T.morts.sim)

## -----------------------------------------------------------------------------
T.morts.fit <- fit_cyclomort(T.morts.sim, n.seasons = 2)

## -----------------------------------------------------------------------------
print(T.morts.fit)

## ----plotSimFits, fig.align = "center"----------------------------------------
plot(T.morts.fit, breaks = 30)

## ---- message = FALSE, echo = 2:6---------------------------------------------
oldpar <- par(no.readonly =TRUE); par(bty = "l", mar = c(4,4,2,2))
T.morts.1season.fit <- fit_cyclomort(T.morts.sim, n.seasons = 1)
T.morts.null.fit <- fit_cyclomort(T.morts.sim, n.seasons = 0)
plot(T.morts.fit, histogram = FALSE, monthlabs = TRUE)
plot(T.morts.1season.fit, histogram = FALSE, add = TRUE, hazcolor = "red")
plot(T.morts.null.fit, histogram = FALSE, add = TRUE, hazcolor = "blue")
par(oldpar)

## ---- eval = 2, echo = 1------------------------------------------------------
timetoeventprediction <- predict(T.morts.fit, t = 1:365, type = "timetoevent", CI = TRUE, nreps = 100)
data(timetoeventprediction)

## ----TimeToEventPrediction, fig.align = "center", echo = 2--------------------
oldpar <- par(no.readonly =TRUE); par(bty = "l", mar = c(4,4,2,2))
with(timetoeventprediction, {
  plot(t, fit, type = "l", ylab = "Time to event", ylim = range(CI), lwd = 2)
  lines(t, CI[1,])
  lines(t, CI[2,])
})
par(oldpar)

## -----------------------------------------------------------------------------
T.select <- select_seasons(T.morts.sim, max.season = 4)

## -----------------------------------------------------------------------------
data(seasonalsex)
str(seasonalsex)

## -----------------------------------------------------------------------------
Survival.factorfit <- factorfit_cyclomort(event ~ sex, data = seasonalsex, n.seasons = 1)
summary(Survival.factorfit)

## ----plotFactorFit, fig.align = "center"--------------------------------------
plot(Survival.factorfit, ymax = 1.2)

## ---- message = FALSE---------------------------------------------------------
data(nwt_morts)
require(ggplot2); require(magrittr); require(plyr)
ggplot(nwt_morts %>% arrange(start) %>% mutate(id = factor(id, levels = id)),
aes(x = start, y = id, col = status)) +
  geom_errorbarh(aes(xmin = start, xmax = end))

## -----------------------------------------------------------------------------
nwt_surv = with(nwt_morts,
                create_cycloSurv(start, end, event = status == "Mort", 
                                 period = 365, timeunit = "days"))

## -----------------------------------------------------------------------------
nwt_fit <- fit_cyclomort(nwt_surv, n.seasons = 3)
plot(nwt_fit, breaks = 40)
summary(nwt_fit)

## ---- message = FALSE---------------------------------------------------------
data(wah_morts)
ggplot(wah_morts %>% arrange(start),
aes(x = start, y = id, col = fate)) + 
  geom_errorbarh(aes(xmin = start, xmax = end))

## -----------------------------------------------------------------------------
wah <- with(wah_morts, create_cycloSurv(start = start, end = end, 
                                        event = fate == "dead", period = 365))

cutoff <- "2016-09-01"
wah_pre = censor_cycloSurv(wah, censor.time = cutoff)
wah_post = trim_cycloSurv(wah, trim.time = cutoff)

wah_fit_pre <- fit_cyclomort(wah_pre, n.seasons = 1)
wah_fit_post <- fit_cyclomort(wah_post, n.seasons = 1)

## -----------------------------------------------------------------------------
summary(wah_fit_pre)
summary(wah_fit_post)

## -----------------------------------------------------------------------------
plot(wah_fit_pre, hist= FALSE, ymax = 0.003, monthlabs = TRUE)
plot(wah_fit_post, hist= FALSE, add = TRUE, hazcolor = "red")

## ---- echo = FALSE------------------------------------------------------------
par(oldpar)

