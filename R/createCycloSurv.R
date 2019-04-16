#'Create an object that can be fitted with a given period from survival (time of death or censoring) data
#'
#'@param start a vector measuring time of birth (as a Date)
#'@param end a vector measuring time of death or censoring (as a Date)
#'@param censoring a vector of booleans (0 = alive, 1 = dead) detailing the status of the observation
#'@param phase a parameter (as a Date) indicating the start of a period. Should be less than any of the values in start
#'@param period length of one period in the input data
#'
#'@return a Surv object with an attribute "period" that reads the period.
#'
#'@example
#'startTimes = as.Date(origin = "2010-01-01", c(0, 0, 0, 50, 0, 50, 100, 150, 0, 100)) #in days
#'endTimes = as.Date(origin = "2010-01-01", c(50, 50, 100, 150, 150, 200, 200, 250, 350)) #in days
#'censored = c(1, 1, 0, 1, 1, 0, 1, 0, 0)
#'phase = "2009-09-01"
#'period = 365
#'morts = createCycloSurv(startTimes, endTimes, censored, phase, period)
#'@export

createCycloSurv = function(start, end, censoring, phase, period) {
  startPhased = as.numeric(difftime(start, phase))
  endPhased = as.numeric(difftime(end, phase))
  result = Surv(time = startPhased, time2 = endPhased, event = censoring)
  attributes(result)$period = period
  attributes(result)$phase = phase
  class(result) = "cycloSurv"
  result
}
