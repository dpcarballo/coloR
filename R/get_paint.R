#' Gets YMCK specifications (paint composition) from a "color" object
#' @param col Color whose complementary is to be calculated. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @return A vector containing col's YMCK (paint) specifications.
#' @examples
#' get_paint("green")
#' get_paint(c(255,0,255))

get_paint <- function(col, maxVal = 255) {
  if(!is_color(col))
    col <- color(col)
  r<- col$red / 255
  g <- col$green / 255
  b <- col$blue / 255
  k <- 1 - max(r,g,b)
  y <- (1-b-k) / (1-k)
  m <- (1-g-k) / (1-k)
  c <- (1-r-k) / (1-k)
  output <- c(y,m,c,k)
  output[is.na(output)] = 0
  names(output) <- c("yellow", "magenta", "cyan", "black")
  return(output)
}
