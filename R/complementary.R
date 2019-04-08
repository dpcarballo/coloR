#' Returns the complementary color to that it receives as input
#' @param ... Color whose complementary is to be calculated. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @return Generally, a hexcode representing the input's complementary. If a "color" class object is received as argument, the complementary will keep this format.
#' @examples
#' complementary("red")
#' complementary("#ff00ff")
#' complementary(0,128,128)
#' complementary(c(0,0,0))
#' complementary(color(c(.7,.25,.25), maxVal=1))
complementary <- function(...) {
  UseMethod("complementary")
}

complementary.character <- function(char) {
  return(hex(complementary(color(char))))
}

complementary.color <- function(color) {
  color$red <- 255 - color$red
  color$green <- 255 - color$green
  color$blue <- 255 - color$blue
  return(color)
}

complementary.numeric <- function(...) {
  if(length(list(...))==1) {
    return(hex(complementary(color(...))))
  } else {
    return(hex(complementary(color(..1,..2,..3))))
  }
}
