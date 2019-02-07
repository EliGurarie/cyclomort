par(oma = c(2,0,2,0));
T.morts1 <- simPeriodicMorts(300, period = 1, 
                             peaks = c(0.25, 0.75), 
                             rhos = c(0.8, 0.5), 
                             weights = 0.3, dt = .01, 
                             A = .02, plotme = TRUE);

with(attributes(T.morts1),
     title(paste0("A: ", A, "; peaks: ",
                  paste(peaks, collapse = ",")), outer = TRUE));

hist(T.morts1[,1] - floor(T.morts1)[,1], breaks = 20)