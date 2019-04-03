#' Corrects a number to be in a given interval
#' @param x Number to be corrected
#' @param min Lower bound of the interval. If x is lower than min, the function will return min.
#' @param max Upper bound of the interval. If x is higher than max, the function will return max.
fit_into_interval <- function(x, min=0, max=255) {
  if(x < min) {
    return(min)
  } else if(x>max) {
    return(max)
  } else {
    return(x)
  }
}