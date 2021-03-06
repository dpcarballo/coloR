#' Creates an object of class "color" as a mixture of the colors it receives as input.
#' @param ... Colors to be mixed. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @param method Type of mixture ("light" or "paint") to be made. Defaults to "light"
#' @param average If TRUE, the function will return the average color of the inputs. If FALSE,
#' the function will add the inputs together. When working with "paint" method, average=TRUE is
#' recommended to represent accurately the mixture of actual paint from idfferent colors.
#' @param output Format in which output will be returned. Use "hex" to get a hexcode. Use "color"
#' for a "color" class object to be returned. Use "paint" for result expression in YMCK palette Defaults to "hex"
#' @return A hexcode, YMCK vector or class "color" object resulting of a mixture of the inputted colors
#' @examples
#' mix("red", "blue", method="light")
#' mix("red", "blue", method="paint")

mix <- function(..., method="light", average=method=="paint", output="hex") {
  if(method=="paint" & !average)
    warning("Usage of paint mixing method without average=TRUE is discouraged!")
  input <- list(...)
  no_color <- !sapply(input, is_color)
  for(i in 1:length(input)) {
    if(no_color[[i]])
      input[[i]] <- color(input[[i]])
  }

  if(method == "paint") {
    spec = numeric(4)
    paints <- lapply(input, get_paint)
    for(i in 1:length(input)) {
      spec <- spec + paints[[i]]
    }
    if(average) {
      spec <- spec/length(input)
    }
    spec[spec > 1] = 1
    result <- paint_color(spec[1], spec[2], spec[3], spec[4], maxVal=1)
  }

  if(method == "light") {
  result <- color(0,0,0)
    for(i in 1:length(input)) {
      result$red <- result$red + input[[i]]$red
      result$green <- result$green + input[[i]]$green
      result$blue <- result$blue + input[[i]]$blue
    }

    if(average) {
      result$red <- round(result$red/length(input))
      result$green <- round(result$green/length(input))
      result$blue <- round(result$blue/length(input))
    }
  }

  if(result$red > 255)
    result$red = 255
  if(result$green > 255)
    result$green = 255
  if(result$blue > 255)
    result$blue = 255


  if(output=="hex") {
    result=hex(result)
  }else if(output=="paint") {
    result = get_paint(result)
  }
  return(result)
}
