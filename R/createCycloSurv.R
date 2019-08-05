#'Create an object that can be fitted with a given period from survival (time of death or censoring) data
#'
#'@param start a vector measuring time of birth (as a POSIXct, numeric, or Date)
#'@param end a vector measuring time of death or censoring (as a POSIXct, numeric, or Date)
#'@param event a vector of booleans (0 = alive, 1 = dead) detailing the status of the observation
#'@param data data.frame object containing start, end and event data
#'@param t0 reference time for event times.  By default, \code{t0} is set to January 1 of the first year of observations (if times are POSIX).
#'@param period length of one period in the input data
#'@param timeunits units that dates are inputted in if dates are being used
#'
#'@return a Surv object with an attribute "period" that reads the period.
#'
#'@examples
#'startTimes = as.Date(origin = "2010-01-01", 
#'                     c(0, 0, 0, 50, 0, 50, 100, 150, 0, 100)) #in days
#'endTimes = as.Date(origin = "2010-01-01", 
#'                   c(50, 50, 100, 150, 150, 200, 200, 250, 350, 500)) #in days
#'censored = c(1, 1, 0, 1, 1, 0, 1, 0, 0, 0)
#'period = 365
#'morts = createCycloSurv(start = startTimes, end = endTimes, 
#'                        event = censored, period = period)
#'@export
#'

createCycloSurv = function(start, end, event, data = NULL, t0 = NULL, period, timeunits = "days") {
  
  if(!is.null(data)){
    cl = match.call()
    parnames = names(cl)
    start_index = match("start", parnames)
    end_index = match("end", parnames)
    event_index = match("event", parnames)
    start = data[as.character(cl[start_index])][,1]
    end = data[as.character(cl[end_index])][,1]
    event = data[as.character(cl[event_index])][,1]
  }
  
  if(is.POSIXct(start)) start <- as.Date(start)
  if(is.POSIXct(end)) end <- as.Date(end)
  if(is.Date(start) & is.Date(end)){
    if(is.null(t0)) t0 <- min(start) - ddays(yday(min(start)))
    if(timeunits == "years"){
      startPhased = as.numeric(difftime(start, t0, units = "days")) / 365.242
      endPhased = as.numeric(difftime(end, t0, units = "days")) / 365.242
    } else {
      startPhased = as.numeric(difftime(start, t0, units = timeunits))
      endPhased = as.numeric(difftime(end, t0, units = timeunits)) 
    }
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
