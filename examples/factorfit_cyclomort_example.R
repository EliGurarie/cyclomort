# fit factorial model
data(seasonalsex)
seasonalsex.factorfit <- factorfit_cyclomort(event ~ sex, data = seasonalsex, n.seasons = 1)

# summary
summary(seasonalsex.factorfit, coefs = TRUE)
\donttest{plot(seasonalsex.factorfit)}
