#' Gets HSV specifications from a "color" object
#' @param col Color whose complementary is to be calculated. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @return A list with col's HSV specifications.
#' @examples
#' get_hsv("red")
#' get_hsv("#00ff00")
#' get_hsv(c(25,25,25))
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
