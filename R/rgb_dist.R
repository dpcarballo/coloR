#' Calculates a measure of the contrast between col1 and col2
#' @param col1 Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
#' @param col2 Hexcode, c(r,g,b), color
#' name as character and "color" class object are all supported.
rgb_dist <- function(col1, col2) {
  UseMethod("rgb_dist")
}

rgb_dist.character <- function(col1, col2) {
  col1 <- color(col1)
  col2 <- color(col2)
  return(rgb_dist(col1,col2))
}

rgb_dist.color <- function(col1, col2) {

  dif <- numeric(3)
  dif[1] <- col1$red - col2$red
  dif[2] <- col1$green - col2$green
  dif[3] <- col1$blue - col2$blue
  ##195075 equals the distance between balck and white.
  return(sum(dif^2)/195075)
}


rgb_dist.numeric <- function(col1, col2) {
  ##Allows for non-integer input
  for(i in 1:3) {
    col1[i]=fit_into_interval(col1[i],0,255)
    col2[i]=fit_into_interval(col2[i],0,255)
  }
  dif <- col1-col2
  return(sum(dif^2)/195075)
}
