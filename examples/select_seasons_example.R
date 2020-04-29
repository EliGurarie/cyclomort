T.morts1 <- simulate_cycloSurv(1000, period = 1, 
                             meanhazard = 0.3, 
                             peaks = c(0.25, 0.75), 
                             durations = c(0.2, 0.1), 
                             weights = c(0.3, 0.7), 
                             plotme = FALSE)

\donttest{
model_selection = select_seasons(T.morts1, max.season = 4)
summary(model_selection$fits)
}