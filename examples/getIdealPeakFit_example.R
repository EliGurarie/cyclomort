T.morts1 <- simPeriodicMorts(300, period = 1, 
                             peaks = c(0.25, 0.75), 
                             rhos = c(0.7, 0.7), 
                             weights = 0.4, dt = .01, 
                             A = .02, plotme = FALSE)

fits = selectNSeasons(T = T.morts1, dt = 0.01)
print(fits)