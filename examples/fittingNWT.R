data(nwt_morts)
# create a CycloSurv object
nwt_surv =  with(nwt_morts,
            create_cycloSurv(start, end, event = status == "Mort",
                            period = 365, timeunit = "days"))
nwt_fit <- fit_cyclomort(nwt_surv, n.seasons = 3)
plot(nwt_fit)
