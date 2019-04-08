#' Calculates a number's mantissa (decimal part, or distance to the nearest lower integer)
#' @param x Number to calculate mantissa.
#' @return A number between 0 and 1 equal to x's mantissa.
#' @examples
#' mantissa(5)
#' mantissa(-5)
#' mantissa(5.3)
#' mantissa(-5.3)

mantissa <- function(x) {
  return(x-floor(x))
}
