#' Standardizes the format of ... arguments. If a list is given, no
#' modification is made. Otherwise, a list is created with all arguments
#' in ...
#' @param ... Input to be converted into a list.
dots_as_list <- function(...) {
  if(class(..1)!="list") {  
    output <- list(...)
  } else {
    output <- (...)
  }
  return(output)
}
