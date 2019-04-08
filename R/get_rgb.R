#' Gets RGB specifications from a "color" object
#' @param col Color whose complementary is to be calculated. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @return A vector containing col's RGB specifications.
#' @examples
#' get_rgb("green")
#' get_rgb(c(255,0,255))
get_rgb <- function(col) {
  if(!is_color(col))
    col <- color(col)
  return(c(col$red, col$green, col$blue))
}
