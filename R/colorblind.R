#' Returns the inputted color as perceivd by a colorblind person
#' @param ... Input color. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @param type Type of color blindness wanted to simulated. "normal", "protanopia", "protanomaly", "deuteranopia",
#' "deuteranomaly","tritanopia", "tritanomaly", "achromatopsia" and "achromatomally" are all supported. Defaults to "deuteranopia" (most common type.
#' @param output Output format. "hex" and "color" supported. Defaults to "hex"
#' @return Generally, a hexcode representing the input's color_blindness. If a "color" class object is received as argument, the color_blindness will keep this format.
#' @examples
#' colorblind("red")
#' colorblind("#ff00ff")
#' colorblind(c(0,0,0))
#' colorblind("#ff00ff", type="tritanomaly")
#'
#'


colorblind <- function(col, type = "deuteranopia", output="hex") {
  if(!is_color(col))
    col=color(col)

  red <- color_blindness_red[type, "red"] * col$red +
         color_blindness_red[type, "green"] * col$green +
         color_blindness_red[type, "blue"] * col$blue

green <- color_blindness_green[type, "red"] * col$red +
         color_blindness_green[type, "green"] * col$green +
         color_blindness_green[type, "blue"] * col$blue

 blue <- color_blindness_blue[type, "red"] * col$red +
         color_blindness_blue[type, "green"] * col$green +
         color_blindness_blue[type, "blue"] * col$blue
 result <- color(red, green, blue)
  if(output=="hex")
    result=hex(result)
  return(result)
}
