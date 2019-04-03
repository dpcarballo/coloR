#' Returns the complementary color to that it receives as input
#' @param ... Color whose complementary is to be calculated. Hexcode, c(r,g,b), color 
#' name as character and "color" class object are all supported.
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