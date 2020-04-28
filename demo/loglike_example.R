T.morts1 <- simulate_cycloSurv(1000, period = 1, 
                             meanhazard = 0.3, 
                             peaks = c(0.25, 0.75), 
                             durations = c(0.2, 0.1), 
                             weights = c(0.5, 0.5), 
                             plotme = FALSE)

peaks = seq(0, 1, length = 30)

##Is there a way to vectorize this function more easily than what I do here?
ll.matrix <- outer(peaks, peaks, function(x,y){
          Vectorize(loglike, args = c())(x = T.morts1, gammas = c(0.15, 0.15), 
                  mus = c(x,y), rhos = findRho(c(.2,.1)))}
          )

image.plot(peaks, peaks, ll.matrix)
