# Example of simulating multi-factor data

eval <- FALSE; if(eval){
  n <- 100
  T.male = simulate_cycloMort(n, period = 1, meanhazard = 0.3, peaks = .25, durations = .3)
  T.female = simulate_cycloMort(n, period = 1, meanhazard = 0.3, peaks = .75, durations = .3)
  T <- with(rbind(T.male, T.female) %>% data.frame,
            createCycloSurv(start = start, end = stop, 
                            event = status, period = 1))
  seasonalsex <- data.frame( sex = rep(c("M","F"), each = n), T = T)
}


# load and visualize simulated sex-specific survival data

data("seasonalsex")

seasonsex.df <- cbind(seasonalsex, as.matrix(seasonalsex$event) %>% as.data.frame) %>%
  arrange(sex,stop) %>% mutate(id = 1:length(start) %>% factor, 
                               status = c("Dead", "Censored")[2-status])
ggplot(seasonsex.df, aes(x = start, y = id, col = sex)) + 
  geom_errorbarh(aes(xmin = start, xmax = stop, lty = status)) + 
    ggtitle("Simualted sex-specific mortality data")

ss.dead <- seasonsex.df %>% subset(status == "Dead") %>% mutate(time = (stop - floor(stop))) 
ggplot(ss.dead, aes(time, y=..density.., fill = sex)) + geom_density(alpha = 0.1) + 
  geom_histogram(alpha=0.5, position="identity")  + 
    ggtitle("Male vs. Female (simulated) mortalities")


# test differences

sex.fit <- factorfit_cyclomort(event ~ sex, data = seasonalsex, n.seasons = 1)
summary(sex.fit)
plot(sex.fit, ymax = 1.3)
