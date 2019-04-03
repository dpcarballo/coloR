#' Creates colors in a gradient between those received as input.
#' @param ... Colors that the user wants to belong to the gradient. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @param steps Number of intermeddiate colors between those specified by the user
#' @param method Procedure to create the gradient. "hsv" and "rgb" are accepted as options.
#' Defaults to "hsv
#' @param output Format in which output will be returned. Use "hex" to get a hexcode. Use "color"
#' for a "color" class object to be returned. Defaults to "Hex
color_gradient <- function(..., steps=1, method="hsv", output="hex") {

  input <- list(...)
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
                                             weights[j])
      }
      index=index+1
    }
  }
  result <- unique(result)

  if(output=="colorlist")
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
