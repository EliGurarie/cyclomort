# fit factorial model
data(seasonalsex)
x <- factorfit_cyclomort(T ~ sex, data = seasonalsex, n.seasons = 1)

# summary
summary(x, coefs = TRUE)
plot(x)

