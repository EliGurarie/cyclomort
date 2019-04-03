#' Print a short summary of cmfit (parameter estimates for periodic mortality curves) objects
#' 
#' @param x a cmfit object
#' @param maxDigits number of digits to round to
#' 
#' @return a short summary of the estimates for each parameter along with confidence intervals and AIC
#' 
#' @example examples/cyclomortFit_example.R
#' @export

print.cmfit = function(x, maxDigits = 4) {
  result = c()
  index = 1
  while (names(x)[index] != "period") {##while x[[index]] is a parameter estimate
    if (names(x)[index] == "meanhazard" || grepl("peak", names(x)[index]) || grepl("duration", names(x)[index]) || grepl("weight", names(x)[index])) {
      estimate = paste0("Estimate for parameter ", names(x)[index], ": ", round(x[[index]][1], digits = maxDigits))
      confidence = paste0("Confidence interval for estimate: [", round(x[[index]][2], digits = maxDigits), ", ", round(x[[index]][3], digits = maxDigits), "]")
      result = c(result, cat(c(estimate, confidence), sep = "\n"))
    }
    index = index + 1
  }
}