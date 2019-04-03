#' Gets hexcode from a "color" object.
#' @param color An object of class "color"
hex <- function(color) {
  r=format(as.hexmode(color$red), width=2)
  g=format(as.hexmode(color$green), width=2)
  b=format(as.hexmode(color$blue), width=2)
  return(paste0("#",r,g,b))
}