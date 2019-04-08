#' Finds as many high-contrast colors as specified by the user.
#' @param ... If desired, user-specified colors that must appear on the list.
#' excode, c(r,g,b), color name as character and "color" class object are all supported.
#' @param n Number of colors to be found.
#' @param remove_white Prevent white from being added to the list?
#' Recommended for usage on white backgrounds. Defaults to TRUE.
#' @param seed Seed to initialize random number generator. Used
#' for consistency between executions.
#' @param maxrep maxrep parameter for maximize_contrast function call
#' @param maxit maxit parameter for maximize_contrast function call
#' @param h h parameter for maximize_contrast function call
#' @param output Output format. "hex" and "color" supported.
#' @return A list of
maximum_contrast_colors <- function(..., n, remove_white=TRUE, seed=1, maxrep=10, maxit=100, h=1e-8, output="hex") {
  set.seed(1)
  if(missing(...) & n<2) {
    warning("Less than two colors were asked for! Returning black")
    if(output=="hex") {
      return(list("#000000"))
    } else {
      return(list(color("#000000")))
    }
  }

  if(missing(...)) {
    result <- list("#ffffff", "#000000")
    if(remove_white) {
      n=n-1
    } else {
      n=n-2
    }
  } else if(remove_white) {
    result <- append("#ffffff", dots_as_list(...))
  } else {
    result <- dots_as_list(...)
  }

  result <- lapply(result, color)
  result <- lapply(result, hex)
  for(i in 1:n) {
    result=append(result, maximize_contrast(result,
                                            maxrep=maxrep,
                                            maxit=maxit,
                                            h=h,
                                            output="hex"))
  }
  if(remove_white)
    result[[1]]=NULL
  if(output=="color")
    result = lapply(result, color)

  return(result)
}
