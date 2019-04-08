#' Checks whether an object belongs to class "color"
#' @param x The object to be checked.
#' @return A logical indicating whether x is a class "color" object.
#' @examples
#' is_color(3)
#' is_color("red")
#' is_color(color("red"))

is_color <- function(x) {
  return(class(x)=="color")
}
