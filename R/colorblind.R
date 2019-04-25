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

  color_blindness_red <- matrix(c(1,0,0,
                                  .57,.43,0,
                                  .82,.18,0,
                                  .63,.37,0,
                                  .8,.2,0,
                                  .95,.05,0,
                                  .97,.03,0,
                                  .3, .59,.11,
                                  .62, .32, .06), nrow=9, ncol=3, byrow = T)

  color_blindness_green <- matrix(c(0,1,0,
                                    .57,.43,0,
                                    .33,.67,0,
                                    .7,.3,0,
                                    .26,.74,0,
                                    0,.43,.57,
                                    0,.73,.27,
                                    .3, .59,.11,
                                    .16, .78, .06), nrow=9, ncol=3, byrow=T)

  color_blindness_blue <- matrix(c(1,0,0,
                                   0, .24, .76,
                                   0, .12, .88,
                                   0, .3, .7,
                                   0, .14, .86,
                                   0, .47, .53,
                                   0, .18, .82,
                                   .3, .59,.11,
                                   .16, .32, .52), nrow=9, ncol=3, byrow=T)


  colnames(color_blindness_red)=c("red", "green", "blue")
  colnames(color_blindness_green)=c("red", "green", "blue")
  colnames(color_blindness_blue)=c("red", "green", "blue")

  rownames(color_blindness_red)=c("normal", "protanopia", "protanomaly", "deuteranopia", "deuteranomaly", "tritanopia", "tritanomaly", "achromatopsia", "achromatomally")
  rownames(color_blindness_green)=c("normal", "protanopia", "protanomaly", "deuteranopia", "deuteranomaly", "tritanopia", "tritanomaly", "achromatopsia", "achromatomally")
  rownames(color_blindness_blue)=c("normal", "protanopia", "protanomaly", "deuteranopia", "deuteranomaly", "tritanopia", "tritanomaly", "achromatopsia", "achromatomally")

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

