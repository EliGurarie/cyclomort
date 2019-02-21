#' Mortality data for Western Arctic Herd Caribou
#'
#' Describing the data.
#'
#' @usage 
#' data(wah_morts)
#' 
#' @format Data frame containing movements of roe deer with the following columns:
#' \describe{
#'   \item{id}{ID of animal}
#'   \item{start}{Date of beginning of collaring}
#'   \item{end}{Date of death or censoring}
#'   \item{Fate}{One of "DEAD", of (censored): "DROPOFF", "CollarFail", "LIVE ANIMAL"}
#'   \item{interval}{In days}}
#' 
#' @examples 
#' data(wah_morts)
#' require(ggplot2)
#' ggplot(wah_morts %>% arrange(start),
#' aes(x = start, y = id, col = Fate)) + 
#'   geom_errorbarh(aes(xmin = start, xmax = end))
#' 
#' @keywords data
#' @references 
"wah_morts"
