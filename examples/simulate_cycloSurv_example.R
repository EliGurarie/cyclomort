par(oma = c(2,0,2,0))
T.morts1 <- simulate_cycloSurv(1000, period = 1, 
                             meanhazard = 0.3, 
                             peaks = c(0.25, 0.75), 
                             durations = c(0.2, 0.1), 
                             weights = 0.3, 
                             plotme = FALSE)

with(attributes(T.morts1),
     title(paste0("mean hazard: ", meanhazard, "; peaks: ",
                  paste(peaks, collapse = ",")), outer = TRUE));



par(oma = c(2,0,2,0))
T.morts2 <- simulate_cycloSurv(300, period = 365, 
                             meanhazard = 0.5/365, 
                             peaks = c(100, 250), 
                             durations = c(20, 40), 
                             weights = 0.4, 
                             plotme = TRUE,
                             max.periods = 5)

with(attributes(T.morts2),
     title(paste0("mean hazard: ", round(meanhazard, 3), "; peaks: ",
                  paste(peaks, collapse = ",")), outer = TRUE));



par(mfrow = c(1,1))
h <- with(as.matrix(T.morts1) %>% data.frame %>% subset(status == 1),
    hist(stop - floor(stop), breaks = 20, col = "grey", bor = "darkgrey"))

with(attributes(T.morts1), curve(mwc(x, mus = peaks, 
                                     rhos = findRho(durations), gammas = weights, 
                                     tau = period)* mean(h$counts), add = TRUE))
