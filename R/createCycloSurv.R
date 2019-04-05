#'Create an object that can be fitted with a given period from survival (time of death or censoring) data
#'
#'@param times a vector of Surv objects measuring time of death or censoring
#'@param censoring a vector of booleans (0 = alive, 1 = dead) detailing 
#'@param period length of one period in "data"
#'
#'@return a Surv object with an attribute "period" that reads the period.
#'
#'@example
#'times = c(50, 50, 100, 150, 150, 200, 200, 250, 350) #in days
#'censored = c(1, 1, 0, 1, 1, 0, 1, 0, 0)
#'period = 365
#'morts = createCycloSurv(times, censored, period)
#'@export

createCycloSurv = function(data, censoring, period) {
  data = data / period #normalized to period == 1 for further analysis
  result = Surv(data, censoring)
  attributes(result)$period = period
  return(result)
}