#' Gets hexcode from a "color" object.
#' @param col Color whose complementary is to be calculated. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @return A string containing col's hex code
hex <- function(col) {
  if(!is_color(col))
    col <- color(col)
  r=format(as.hexmode(col$red), width=2)
  g=format(as.hexmode(col$green), width=2)
  b=format(as.hexmode(col$blue), width=2)
  return(paste0("#",r,g,b))
}
