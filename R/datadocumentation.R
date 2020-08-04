#' Mortality data for Western Arctic Herd Caribou
#'
#' Anonymized mortality data on Western Arctic Herd caribou collected by the 
#' U.S. National Park Service, Alaska, with grateful acknowledgments to K. Joly. 
#'
#' @usage 
#' data(wah_morts)
#' 
#' @format Data frame with 171 rows and the following columns:
#' \describe{
#'   \item{id}{ID of animal}
#'   \item{start}{Date of beginning of collaring}
#'   \item{end}{Date of death or censoring}
#'   \item{fate}{One of "dead", or "censored"}
#' }
#' 
#' @examples 
#' data(wah_morts)
#' require(ggplot2); require(magrittr); require(plyr)
#' ggplot(wah_morts %>% arrange(start),
#' aes(x = start, y = id, col = fate)) + 
#'   geom_errorbarh(aes(xmin = start, xmax = end))
#'
#' @source U.S. National Park Service, Alaska
#' @keywords data
"wah_morts"



#' Mortality data for Northwest territory boreal woodland caribou.
#'
#' Mortality data for Northwest territory boreal woodland caribou, anonymized 
#' and randomized by year, thereby retaining the multi-seasonal signal without, 
#' with grateful acknowledgements to A. Kelly and N. Larter. 
#'
#' @usage
#' data(nwt_morts)
#'
#' @format Data frame with 370 rows and the following columns:
#' \describe{
#'   \item{id}{ID of animal}
#'   \item{start}{Date of beginning of collaring}
#'   \item{end}{Date of death or censoring}
#'   \item{status}{"Mort" or "Cens" (dead or censored)}
#'   }
#'
#' @examples
#' data(nwt_morts)
#' require(ggplot2); require(magrittr); require(plyr)
#' ggplot(nwt_morts %>% arrange(start) %>% mutate(id = factor(id, levels = id)),
#' aes(x = start, y = id, col = status)) +
#'   geom_errorbarh(aes(xmin = start, xmax = end))
#'
#' @source Government of Northwest Territories, Canada
#' @keywords data
"nwt_morts"


#' Simulated data of seasonal mortality data for two sex groups
#' 
#' See examples below for the process of simulating and visualizing these data 
#' using \code{\link{simulate_cycloSurv}}, and an example of analyzing these 
#' data with \code{\link{factorfit_cyclomort}}.
#'
#' @usage data(seasonalsex)
#' 
#' @format Simulated data of single-season mortalities for two sex groups:
#' \describe{
#'   \item{sex}{female (F) or male (M)}
#'   \item{event}{\code{cycloSurv} object of (censored) survival data}
#' }
#' @example examples/seasonalsex_example.R
#' @keywords data
"seasonalsex"



#' Example fitted time to event prediction
#'
#' @usage
#' data(timetoeventprediction)
#' @keywords data
#' @examples
#' \donttest{
#' ## Code that generates this object - following example in vignette
#' set.seed(10)
#' T.morts.sim <- simulate_cycloSurv(300, period = 365, meanhazard = 1/365, 
#' peaks = c(100, 250), durations = c(25, 40), weights = c(0.4, 0.6), plotme = FALSE)
#' T.morts.fit <- fit_cyclomort(T.morts.sim, n.seasons = 2)
#' timetoeventprediction <- predict(T.morts.fit, t = 1:365, 
#'                                  type = "timetoevent", CI = TRUE, nreps = 100)
#' }
#' data(timetoeventprediction)
#' with(timetoeventprediction, {
#' plot(t, fit, type = "l", ylab = "Time to event", ylim = range(CI), lwd = 2)
#' lines(t, CI[1,])
#' lines(t, CI[2,])})
"timetoeventprediction"