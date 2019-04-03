#' Gets HSV specifications from a "color" object
#' @param col An object of class color.
get_hsv <- function(col) {
  if(!is_color(col))
    col <- color(col)
  hsv <- grDevices::rgb2hsv(col$red, col$green, col$blue)
  result <- list()
  result$h <- hsv["h",1]
  result$s <- hsv["s",1]
  result$v <- hsv["v",1]
  return(result)
}
