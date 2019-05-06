n = 100

T.male = simPeriodicMorts(n, period = 365, 
                             meanhazard = 0.3 / 365, 
                             peaks = c(0.25 * 365), 
                             durations = c(0.3 * 365), 
                             weights = 0.7, 
                             plotme = FALSE)
T.female = simPeriodicMorts(n, period = 365, 
                            meanhazard = 0.3 / 365, 
                            peaks = c(0.75 * 365), 
                            durations = c(0.3 * 365), 
                            weights = 0.7, 
                            plotme = FALSE)

raw_morts = rbind(T.male, T.female)
T.morts1 = createCycloSurv(start = raw_morts[,1], end = raw_morts[,2], 
                           event = raw_morts[,3], period = 365)

sex = as.factor(c(rep("M", n), c(rep("F", n))))
data = data.frame(surv = T.morts1, factor = sex)

model = model_cyclomort(formula = T.morts1 ~ sex, 
                        data = data, 
                        n.seasons = 1)
summary(model)
