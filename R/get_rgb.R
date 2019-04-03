#' Gets RGB specifications from a "color" object
#' @param col An object of class color.
get_rgb <- function(col) {
  if(!is_color(col))
    col <- color(col)
  return(c(col$red, col$green, col$blue))
}
