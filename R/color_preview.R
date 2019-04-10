#' Previsualizes colors in a plot window
#' @param ... Colors to be displayed.
#' @examples
#' color_preview(colorblind("red", type="protanopia"), colorblind("green", type="protanopia"))
#' color_preview("#000000", "#333333", "#666666", "#999999", "#bbbbbb", "#eeeeee")
color_preview <- function(...) {
  plot(-1,-1,xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="")

  input <- dots_as_list(...)
  input <- sapply(input, hex)
  n <- length(input)

  for(i in 1:n) {
    polygon(x=c((i-1)/n, i/n, i/n, (i-1)/n),
            y=c(0,0,1,1),
            col = input[[i]])
  }
}
