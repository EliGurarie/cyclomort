##Turn WAH mortality data into a Surv object for fitting purposes

data(wah_morts)
require(lubridate)
wah_morts <- subset(wah_morts, end < ymd("2017 9 1"))

PERIOD_LENGTH = 365

wah = wah_morts[,c(4,5)]
wah[,1] = numeric(length(wah[,1]))
wah[(as.character(wah_morts[,4]) != "DEAD"),1] = 0
wah[(as.character(wah_morts[,4]) == "DEAD"),1] = 1

wah[,2] = wah[,2] / PERIOD_LENGTH

attributes(wah)$period = 1 #measuring it in years, why not?

wah = Surv(wah[,2], wah[,1])

p0 = c(A = 0.05, peak1 = 0.25, peak2 = 0.75, rho1 = 0.5, rho2 = 0.5, weight1 = 0.5)

fits = fit_cyclomort(T = wah, p0 = p0, dt = 0.01)

##Slight error with confidence intervals - we get estimates that work but the CIs end up being NA for most of the parameters (all but A and peak1).
####The command sqrt(diag(solve(hessian))) is the issue because diag(solve(hessian)) has some negative values in it in this case.
####I don't know why this is but maybe there aren't enough data points? I have no idea. For now I've "fixed" the intervals so that they're set to [0,1] in this case.

plot(fits)

myhist <- hist(wah[,1][wah[,2] == 1] %% 1, breaks = 30, col = "grey")
ts <- seq(0,1,.01)
h <- getHazard(ts, fits)
lines(ts, h/max(h) *   max(myhist$counts), lwd = 2)
s1.low <- with(fits, peak1 - rho1season/2)[1]
s1.high <- with(fits, peak1 + rho1season/2)[1]
s2.low <- with(fits, peak2 - rho2season/2)[1]
s2.high <- with(fits, peak2 + rho2season/2)[1]

abline(v = c(s1.low, s1.high, s2.low, s2.high), col = 1:4)


