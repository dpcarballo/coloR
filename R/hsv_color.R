#' Creates an object of class "color" from HSV palette specifications
#' @param h Hue
#' @param s Saturation
#' @param v Value
#' @param vmax Maximum permitted input for value. Defaults to 1
#' @param hmax Perimeter of the colour circle. Defaults to 1
#' @return The corresponding object of class "color".
#' @examples
#' hsv_color(0,1,1)
#' hsv_color(3.1416, 1, 1, hmax=2*pi)
#' hsv_color(.5, .5, 128, vmax=255)
hsv_color <- function(h,s,v, hmax=1, vmax=1) {

  v=v/vmax
  h=h/hmax
  if(h>1)
    h = mantissa(h)
  return(color(hsv(h,s,v)))
}
