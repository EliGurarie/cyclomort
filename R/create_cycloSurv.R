#'Create a cycloSurv object
#'
#' \code{cycloSurv} is a superclass of \code{Surv}, the standard data type for
#' survival analysis in R, with an additional period attribute necessary for 
#' estimating periodic hazard functions. 
#'
#'@param start a vector measuring time an individual enters a population
#'(can be POSIX, numeric, or Date)
#'@param end a vector measuring time an individual leaves a population, e.g. via 
#'death (or other precipitation event of interest) or censoring. 
#'(as a POSIXct, numeric, or Date)
#'@param event the status indicator, normally 0=alive/censored, 1=dead. 
#'@param t0 reference time for event times.  By default, \code{t0} is set to 
#'January 1 of the first year of observations if times are POSIXct. There are 
#'many reasons why a biological year may more conveniently start on a different 
#'day.  All else being equal, it can be useful to start a "mortality year" at a 
#'period of low mortality to better isolate the seasons of higher mortality. 
#'@param period length of one period in the input data
#'@param timeunits units that dates are inputted in if dates are being used
#'
#'@return an object of class \code{cycloSurv} which is identical to and 
#'compatible with a '\code{Surv} object, with, however, an addition "period" 
#'attribute. 
#'
#'@examples
#'startTimes = as.Date(origin = "2010-01-01", 
#'                     c(0, 0, 0, 50, 0, 50, 100, 150, 0, 100)) #in days
#'endTimes = as.Date(origin = "2010-01-01", 
#'                   c(50, 50, 100, 150, 150, 200, 200, 250, 350, 500)) #in days
#'censored = c(1, 1, 0, 1, 1, 0, 1, 0, 0, 0)
#'period = 365
#'morts = create_cycloSurv(start = startTimes, end = endTimes, 
#'                        event = censored, period = period)
#'@export

create_cycloSurv = function(start, end, event, t0 = NULL, period, 
                            timeunits = "days") {
  
  if(lubridate::is.POSIXct(start)) start <- as.Date(start)
  if(lubridate::is.POSIXct(end)) end <- as.Date(end)
  if(lubridate::is.Date(start) & lubridate::is.Date(end)){
    if(is.null(t0)) t0 <- min(start) - 
        lubridate::ddays(lubridate::yday(min(start)))
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
  attributes(result)$timeunits = timeunits
  class(result) = c("cycloSurv", "Surv")
  result
}
