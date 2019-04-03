#' Calculates a number's mantissa (decimal part, or distance to the nearest lower integer)
#' @param x Number to calculate mantissa.
mantissa <- function(x) {
  return(x-floor(x))
}