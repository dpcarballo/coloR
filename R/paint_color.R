#' Creates an object of class "color" from a mixture of paint primary colors (YMCK palette)
#' @param y Amount of yellow paint
#' @param m Amount of magenta paint
#' @param c Amount to cyan light
#' @param k Amount of black paint. Defaults to 0
#' @param maxVal Value assigned to represent maximum amount of paint. Defaults to 255
#' @return An object of class color.
#' @examples
#' paint_color(255,255,0)
#' paint_color(255,255,0,128)
#' paint_color(0, 1, 1, maxVal=1)
paint_color <- function(y, m, c, k=0, maxVal=255) {
  if(y>maxVal)
    warning("yellow exceedes maximum allowed value")
  if(m>maxVal)
    warning("magenta exceedes maximum allowed value")
  if(c>maxVal)
    warning("cyan exceedes maximum allowed value")
  y <- y/maxVal
  m <- m/maxVal
  c <- c/maxVal
  k <- k/maxVal
  r <- (1-c) * (1-k)
  g <- (1-m) * (1-k)
  b <- (1-y) * (1-k)
  return(color(r,g,b,maxVal=1))
}
