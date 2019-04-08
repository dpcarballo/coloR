#' Standardizes the format of ... arguments. If a list is given, no
#' modification is made. Otherwise, a list is created with all arguments
#' in ...
#' @param ... Input to be converted into a list.
#' @return A list containing the ... arguments regardless of whether they originally were a list or not.
#' @examples
#' identical(dots_as_list(3,4,5), dots_as_list(list(3,4,5)))
dots_as_list <- function(...) {
  if(class(..1)!="list") {
    output <- list(...)
  } else {
    output <- (...)
  }
  return(output)
}
