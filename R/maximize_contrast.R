#' Finds the highest contrast color available for those specified by the user.
#' Uses gradient-descent optimization on the "contrast" function.
#' @param ... All the colors of interest. Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @param maxrep Number of times the process of local optimization will be repeated. Defaults to 10
#' @param maxit Number of iterations for each gradient descent optimization (finding of local minimum). Defaults to 100
#' @param h Step used to calculate the gradient of the contrast function. Defaults to 1e-8
#' @return Either a hexcode or class "color" object with highest contrast with those inputted.
#' @examples
#' maximize_contrast("green")
#' maximize_contrast("yellow", "red")
#' maximize_contrast(list("yellow", "red", c(0,255,0)))

maximize_contrast <- function(...,maxrep=10, maxit=100, h=1e-8, output="hex") {
  lider <- color(128,128,128)
  for(rep in 1:maxrep) {
    x <- sample(255,3,rep=T)
    for(it in 1:maxit) {
      d <- numeric(3)
      d[1] <- (contrast(c(x[1]+h,x[2],x[3]),...) - contrast(c(x[1]-h,x[2],x[3]),...))/(2*h)
      d[2] <- (contrast(c(x[1],x[2]+h,x[3]),...) - contrast(c(x[1],x[2]-h,x[3]),...))/(2*h)
      d[3] <- (contrast(c(x[1],x[2],x[3]+h),...) - contrast(c(x[1],x[2],x[3]-h),...))/(2*h)
      d = d/sqrt(sum(d^2))
      step <- optimal_step(x,d,...)
      if(step<1e-7)
        break
      x=x+d*step
      x=sapply(x, fit_into_interval)
    }
    if(contrast(x,...) > contrast(lider,...)) {
      lider=x
    }
  }
  lider=color(lider)
  if(output=="hex")
    lider=hex(lider)
  return(lider)
}
