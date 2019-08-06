T.morts1 <- simulate_cycloSurv(1000, period = 1, 
                             meanhazard = 0.3, 
                             peaks = c(0.25, 0.75), 
                             durations = c(0.2, 0.1), 
                             weights = 0.5, 
                             plotme = FALSE)

peaks = seq(0, 1, length = 30)

##Is there a way to vectorize this function more easily than what I do here?
ll.matrix = matrix(nrow = 30, ncol = 30, 0)
for (i in 1:30) {
  for (j in 1:30) {
    ll.matrix[i, j] = loglike(T.morts1, 
                              gammas = c(0.15, 0.15), 
                              mus = c(peaks[i], peaks[j]), 
                              rhos = findRho(c(.2,.1)))
  }
}
image.plot(peaks, peaks, ll.matrix)
