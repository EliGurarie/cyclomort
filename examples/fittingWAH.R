##Turn WAH mortality data into a Surv object for fitting purposes

data(wah_morts)

START_TIME = "2010-08-01"
CENSOR_TIME = "2016-09-01"
difference = difftime(CENSOR_TIME, START_TIME, units = "days")

wah = create_cycloSurv(start = wah_morts$start, end = wah_morts$end, event = wah_morts$fate == "dead", period = 365, t0 = START_TIME)
wah_pre2017 = censor.cycloSurv(wah, censor.times = difference)
wah_post2017 = trim.cycloSurv(wah, trim.times = difference)

##For Pre-2017 Data

wah_fits_pre2017 = list(
  NullModel = try(fit_cyclomort(wah_pre2017, n.seasons = 0)),
  OneSeason = try(fit_cyclomort(wah_pre2017, n.seasons = 1)),
  TwoSeasons = try(fit_cyclomort(wah_pre2017, n.seasons = 2)),
  ThreeSeasons = try(fit_cyclomort(wah_pre2017, n.seasons = 3)),
  FourSeasons = try(fit_cyclomort(wah_pre2017, n.seasons = 4)))

aic_pre2017 <- sapply(wah_fits_pre2017, AIC)
wah_pre2017_bestFit = wah_fits_pre2017[[which.min(aic_pre2017)]]

par(bty="l", mar = c(4,4,4,0), 
    tck = 0.1, cex.axis = 0.8, mgp = c(1.75, .5, 0), tck = 0.01, las = 1)
layout(t(c(1,2)), widths = c(0.4,0.6))
par(las = 0)
plot(0:4, aic_pre2017, xlab = "Number of seasons", ylab = "fitted model AIC", type = "o")
plot(wah_pre2017_bestFit, breaks = 30)

##For Post-2017 Data
####It should be noted that this set has less than 30 data points in it. When I ran the
####analysis here all the fits failed!

wah_fits_post2017 = list(
  NullModel = try(fit_cyclomort(wah_post2017, n.seasons = 0)),
  OneSeason = try(fit_cyclomort(wah_post2017, n.seasons = 1)),
  TwoSeasons = try(fit_cyclomort(wah_post2017, n.seasons = 2)),
  ThreeSeasons = try(fit_cyclomort(wah_post2017, n.seasons = 3)),
  FourSeasons = try(fit_cyclomort(wah_post2017, n.seasons = 4)))

aic_post2017 <- sapply(wah_fits_post2017, AIC)
wah_post2017_bestFit = wah_fits_post2017[[which.min(aic_post2017)]]

par(bty="l", mar = c(4,4,4,0), 
    tck = 0.1, cex.axis = 0.8, mgp = c(1.75, .5, 0), tck = 0.01, las = 1)
layout(t(c(1,2)), widths = c(0.4,0.6))
par(las = 0)
plot(0:4, aic_post2017, xlab = "Number of seasons", ylab = "fitted model AIC", type = "o")
plot(wah_post2017_bestFit, breaks = 30)