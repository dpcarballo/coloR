#' Finds a color between those received as input in HSV palette
#' @param col1 First color. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @param col2 Second color. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @param w1 Number between 0 and 1. Represents how similar the output will be to col1. Defaults to .5
#' @param w2 Number between 0 and 1. Represents how similar the output will be to col2. Defaults to 1-w1
#' @param output Format in which output will be returned. Use "hex" to get a hexcode. Use "color"
#' for a "color" class object to be returned. Defaults to "hex"
#' @return A hexcode (or "class" color object) built as commanded.
#' @examples
#' hsv_weighted_mean("red", "green", output="color")
#' hsv_weighted_mean("red", "green")
#' hsv_weighted_mean("blue", "red", w1=5/6)
#' hsv_weighted_mean(c(0,0,0), color("white"))
#' hsv_weighted_mean("#000000", "red")
hsv_weighted_mean <- function(col1, col2, w1=.5, w2=1-w1, output="hex") {
  if(!is_color(col1))
    col1 <- color(col1)
  if(!is_color(col2))
    col2 <- color(col2)
  hsv1 <- get_hsv(col1)
  hsv2 <- get_hsv(col2)
  if(hsv2$h >= hsv1$h) {
    h <- w1*hsv1$h + w2*hsv2$h
  } else {
    h <- w1*(1+hsv1$h) + w2*hsv2$h
  }
  s <- w1*hsv1$s + w2*hsv2$s
  v <- w1*hsv1$v + w2*hsv2$v
  col <- hsv_color(h,s,v, vmax=1, hmax=1)
  if(output=="hex")
    col=hex(col)
  return(col)
}
