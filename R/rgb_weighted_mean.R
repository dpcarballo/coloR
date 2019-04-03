#' Finds a color between those received as input in RGB palette
#' @param col1 First color. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @param col2 Second color. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @param w1 Number between 0 and 1. Represents how similar the output will be to col1. Defaults to .5
#' @param w2 Number between 0 and 1. Represents how similar the output will be to col2. Defaults to 1-w1
#' @param output Format in which output will be returned. Use "hex" to get a hexcode. Use "color"
#' for a "color" class object to be returned. Defaults to "hex"
rgb_weighted_mean <- function(col1, col2, w1=.5, w2=1-w1, output="hex") {
  if(!is_color(col1))
    col1 <- color(col1)
  if(!is_color(col2))
    col2 <- color(col2)
  r <- w1*col1$r + w2*col2$r
  g <- w1*col1$g + w2*col2$g
  b <- w1*col1$b + w2*col2$b
  col <- color(r,g,b, maxVal = 255)
  if(output=="hex")
    col=hex(col)
  return(col)
}
