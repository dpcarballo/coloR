#' Creates an object of class "color" from HSV palette specifications
#' @param h Hue
#' @param s Saturation
#' @param v Value
#' @param vmax Maximum permitted input for value. Defaults to 1
#' @param hmax Perimeter of the colour circle. Defaults to 1
hsv_color <- function(h,s,v, vmax=1, hmax=1) {
  
  v=v/vmax
  h=h/hmax
  if(h>1)
    h = mantissa(h)
  return(color(hsv(h,s,v)))
}
