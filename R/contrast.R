#' Calculates the minimum contrast between col and the colors specified in ...
#' @param col The color we want to compare. Hexcode, c(r,g,b), color 
#' name as character and "color" class object are all supported.
#' @param ... All other colors that col will be compared to. Hexcode, c(r,g,b), color 
#' name as character and "color" class object are all supported.
contrast <- function(col,...) {
  input <- dots_as_list(...)
  if(!is.numeric(col))
    col=get_rgb(col)
  input <- lapply(input, get_rgb)
  return(min(sapply(input,rgb_dist, col2=col)))
}