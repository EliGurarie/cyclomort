#' @title Mortality data for Western Arctic Herd Caribou
#'
#' Describing the data.
#'
#' @usage 
#' data(wah_morts)
#' 
#' @format Data frame with 171 rows and the following columns:
#' \describe{
#'   \item{id}{ID of animal}
#'   \item{start}{Date of beginning of collaring}
#'   \item{end}{Date of death or censoring}
#'   \item{Fate}{One of "DEAD", of (censored): "DROPOFF", "CollarFail", "LIVE ANIMAL"}
#' }
#' 
#' @examples 
#' data(wah_morts)
#' require(ggplot2)
#' ggplot(wah_morts %>% arrange(start),
#' aes(x = start, y = id, col = Fate)) + 
#'   geom_errorbarh(aes(xmin = start, xmax = end))
#'
#' @source U.S. National Park Service, Alaska
#' @keywords data
"wah_morts"

#' #' Mortality data for Northwest territory boreal woodland caribou.
#' #'
#' #' 
#' #' @usage 
#' #' data(nwt_morts)
#' #' 
#' #' @format Data frame with 370 rows and the following columns:
#' #' \describe{
#' #'   \item{id}{ID of animal}
#' #'   \item{region}{Subpopulation marker - for comparisons}
#' #'   \item{start}{Date of beginning of collaring}
#' #'   \item{end}{Date of death or censoring}
#' #'   \item{status}{"Mort" or "Cens" (dead or censored)}
#' #' 
#' #' @examples 
#' #' data(nwt_morts)
#' #' require(ggplot2)
#' #' ggplot(nwt_morts %>% arrange(start) %>% mutate(id = factor(id, levels = id)),
#' #' aes(x = start, y = id, col = status)) + 
#' #'   geom_errorbarh(aes(xmin = start, xmax = end))
#' #' 
#' #' @source Government of Northwest Territories, Canada
#' #' @keywords data
#' #' @references 
#' "nwt_morts"

#' Simulated data of seasonal mortality data for two sex groups
#' 
#' See examples below for the process of simulating and visualizing these data using \code{\link{simPeriodicMorts}}, and an example of analyzing these data with \code{\link{factorfit_cyclomort}}.
#'
#' @usage data(seasonalsex)
#' 
#' @format Simulated data of single-season mortalities for two sex groups:
#' \describe{
#'   \item{sex}{female (F) or male (M)}
#'   \item{T}{\code{\link{CycloSurv}} object of (censored) survival data}
#' }
#' @example examples/seasonalsex_example.R
#' @keywords data
"seasonalsex"
