require(fields);

T.morts1 <- simPeriodicMorts(300, period = 1, 
                             peaks = c(0.25, 0.75), 
                             rhos = c(0.8, 0.5), 
                             weights = 0.3, dt = .01, 
                             A = .02, plotme = FALSE);

peaks = seq(0, 1, length = 30);
##Is there a way to vectorize this function more easily than what I do here?
ll.matrix = matrix(nrow = 30, ncol = 30, 0);
for (i in 1:30) {
  for (j in 1:30) {
    ll.matrix[i, j] = loglike(T.morts1, A = 0.02, p = c(peaks[i], peaks[j]), r = c(0.8, 0.5), w = c(0.3), 0.01, period = 1);
  }
}

image.plot(peaks, peaks, ll.matrix)