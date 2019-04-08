#' Creates colors in a gradient between those received as input.
#' @param ... Colors that the user wants to belong to the gradient. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported. Inputting all of them as a list is also allowed
#' @param steps Number of intermeddiate colors between those specified by the user
#' @param smooth_factor Parameter smooth_factor hor rgb_weighted_mean call. Defaults to 2
#' @param method Procedure to create the gradient. "hsv" and "rgb" are accepted as options.
#' Defaults to "hsv
#' @param output Format in which output will be returned. Use "hex" to get a hexcode. Use "color"
#' for a "color" class object to be returned. Defaults to "hex"
#' @return By default, character version containing hexcodes for every color belonging in the gradient. Using "color" as output, a list of class "color" objects representing the gradient
#' @examples
#' color_gradient("black", "red", "white", steps=3, method="rgb")
#' color_gradient("red", "green", "blue")
#' color_gradient("black","white", steps=11)
color_gradient <- function(..., steps=1, smooth_factor=2, method="hsv", output="hex") {

  input <- dots_as_list(...)
  if(length(input)<2)
    stop("Please provide at least two colors")
  no_color <- !sapply(input, is_color)
  for(i in 1:length(input)) {
    if(no_color[[i]])
      input[[i]] <- color(input[[i]])
  }

  result <- list()
  weights <- seq(1,0,-1/(steps+1))
  index <- 1
  for(i in 1:(length(input)-1)) {
    for(j in 1:(steps+2)) {
      if(method=="hsv") {
        result[[index]] <- hsv_weighted_mean(input[[i]], input[[i+1]],
                                             weights[j])
      } else if(method=="rgb") {
        result[[index]] <- rgb_weighted_mean(input[[i]], input[[i+1]],
                                             weights[j], smooth_factor = 2)
      }
      index=index+1
    }
  }
  result <- unique(result)

  if(output %in% c("color", "colorlist"))
    result <- lapply(result, color)
  if(output=="hex") {
    rprov <- result
    result <- character(length(rprov))
    for(i in 1:length(result)) {
      result[i] <- rprov[[i]]
    }
  }
  return(result)
}
