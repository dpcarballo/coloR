#' Calculates best possible step for contrast gradient descent optimization.
#' @param x Point of origin
#' @param d Direction of gradient
#' @param ... Colors to compare with x (calculate contrast with x)
#' @param pasos Candidate steps to choose from
#' @return A number equal to best step in the list
optimal_step <- function(x,d,...,
                         pasos=c(0,1e-8,2e-8,5e-8,1e-7,2e-7,5e-7,1e-6,2e-6,5e-6,1e-5,2e-5,5e-5,1e-4,2e-4,5e-4,1e-3,2e-3,5e-3,1e-2,2e-2,5e-2,0.1,0.2,0.5,1,2,5,10, 20, 50,100)) {
  rendimiento_pasos <- numeric(length(pasos))
  for(i in 1:length(pasos))
    rendimiento_pasos[i] = contrast(x+pasos[i]*d, ...)

  return(pasos[which.max(rendimiento_pasos)])
}
