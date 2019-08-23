#' Cyclomort: periodic survival modeling
#' 
#' @description This package allows users to estimate parameteric hazard functions with a known periodicity and one of more peak seasons of heightened mortality risk. It was motivated by the strongly seasonal mortality signal observed in many wild animal populations, but the model may be useful for any periodic time-to-event process.  
#' 
#' 
#' @details #' The central estimation function \code{\link{fit_cyclomort}} produces esimates for timing, duration and intensity of mortality peaks from right-censored survival data.  Other functions simulate survival data from periodic hazard functions (\code{\link{simulate_cycloSurv}}), perform model selection to identiy the number of seasons (\code{\link{select_seasons}}), perform simple hypothesis tests (\code{\link{factorfit_cyclomort}}), and various methods for visualizing and summarizing fits and model predictions.  Several data sets are also included. 
#' 
#' Details of the underlying model, motivation, and examples of implementation on mortality data are in the Gurarie et al. (*in review*). An active development version is on GitHub at https://github.com/EliGurarie/cyclomort.  The vignette provides several examples of the functionality of the package.
#' 
#' @keywords internal
#' @references E. Gurarie, P. Thompson, A. Kelly, N. Larter, W. Fagan and K. Joly. \emph{in review} For Everything There is a Season: Estimating periodic hazard functions with the cyclomort R package. \emph{Methods in Ecology and Evolution}.
#' 
#' @import stats
#' @import magrittr
#' @import plyr
#' @import mvtnorm
#' @import survival
#' @import flexsurv
#' @import graphics
#' 
"_PACKAGE"