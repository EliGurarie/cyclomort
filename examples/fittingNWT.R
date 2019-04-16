data(nwt_morts)

# create a CycloSurv object

NWT <- Surv(nwt_morts$interval, nwt_morts$status == "Mort")
NWT[,1] = NWT[,1]/365
attributes(NWT)$period = 365

hist(yday(nwt_morts[nwt_morts$status == "Mort",]$end), breaks = 20)

null_estimate = flexsurvreg(NWT ~ 1, dist = "exp")

p0 = c(meanhazard = 0.1751, peak1 = 100/365, peak2 = 200/365, peak3 = 300/365, 
       duration1 = 20/365, duration2 = 20/365, duration3 = 20/365,
       weight1 = 0.3, weight2 = 0.3)

list(mus = c(1,2), omegas = c(.5,.5)) %>% unlist

system.time(optim(p0, loglike_optim, T = NWT, hessian = TRUE, method =  "L-BFGS-B",
             lower = c(meanhazard = 0, 
                       peak1 = 0, peak2 = 0, peak3 = 0,
                       duration1 = 0, duration2 = 0, duration3 = 0,
                       weight1 = 0, weight2 = 0) + 1e-6,
             upper = c(meanhazard = Inf, 
                       peak1 = attributes(NWT)$period, 
                       peak2 = attributes(NWT)$period, 
                       peak3 = attributes(NWT)$period,
                       duration1 = .5, 
                       duration2 = .5, 
                       duration3 = .5,
                       weight1 = 1, weight2 = 1)
             ))
