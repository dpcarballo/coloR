#' Creates an object of class 'color'
#' @param ... A character containing a color name, a hexcode, a c(r,g,b) vector or
#' three single numbers representing r,g,b respectively
#' @param maxVal Value assigned to represent maximum amount of light. Defaults to 255
#' @return An object of class "color" made from specifications given in the arguments
#' @examples
#' color(255,0,0)
#' color("magenta")
#' color(c(0,255,0))
#' color("#0000ff")

color <- function(..., maxVal=255) {
  col = list()
  class(col)="color"

  input=list(...)
  #Function behaives differently depending of number of arguments
  if(length(input)==3) {
    #color(255,0,0)
    r=input[[1]]
    g=input[[2]]
    b=input[[3]]
  } else if(length(input) == 1) {
    input=input[[1]]
    #Naming simplification
    if(is_color(input)) {
      return(input)
    } else if(is.numeric(input)) {
      #color(c(255,0,0))
      r=input[1]
      g=input[2]
      b=input[3]
    } else if(is.character(input)) {
      if(nchar(input)==7 & substr(input, 1,1) == "#") {
        #color("#FF0000")
        r=strtoi(paste0("0x", substr(input, 2, 3)))
        g=strtoi(paste0("0x", substr(input, 4, 5)))
        b=strtoi(paste0("0x", substr(input, 6, 7)))
      } else { #color("red")
        return(color(gplots::col2hex(input)))
      }
    }
  }

  r <- fit_into_interval(r,0,maxVal)
  g <- fit_into_interval(g,0,maxVal)
  b <- fit_into_interval(b,0,maxVal)

  coef = 255/maxVal
  col$red=round(r*coef)
  col$green=round(g*coef)
  col$blue=round(b*coef)
  return(col)
}
