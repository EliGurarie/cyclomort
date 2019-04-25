#'Create an object that can be fitted with a given period from survival (time of death or censoring) data
#'
#'@param start a vector measuring time of birth (as a Date)
#'@param end a vector measuring time of death or censoring (as a Date)
#'@param event a vector of booleans (0 = alive, 1 = dead) detailing the status of the observation
#'@param t0 start time for all times (start and end).  Note, the selection of t0 is related to the phase shift of the periodic analysis and should be set to the period of lowest hazard as determined, e.g., from a visual analysis.
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

createCycloSurv = function(start, end, event, t0 = NULL, period, timeunits = "days") {
  
  if(is.POSIXct(start)) start <- as.Date(start)
  if(is.POSIXct(end)) end <- as.Date(end)
  if(is.Date(start) & is.Date(end)){
    if(is.null(t0)) t0 <- min(start) - ddays(yday(min(start)))
    startPhased = as.numeric(difftime(start, t0, units = timeunits))
    endPhased = as.numeric(difftime(end, t0, units = timeunits))
  } else if(is.numeric(start) & is.numeric(end)){
    if(is.null(t0)) t0 = 0
    startPhased = start - t0
    endPhased = end - t0
  } else 
    stop("Invalid data type: 'start' and 'end' must both be time (POSIX), date (Date), or numeric objects.\n")
    
  result = Surv(time = startPhased, time2 = endPhased, event = event)
  attributes(result)$period = period
  attributes(result)$t0 = t0
  class(result) = c("cycloSurv", "Surv")
  result
}
