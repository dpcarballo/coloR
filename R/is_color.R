#' Checks whether an object belongs to class "color"
#' @param x The object to be checked.
is_color <- function(x) {
  return(class(x)=="color")
}
