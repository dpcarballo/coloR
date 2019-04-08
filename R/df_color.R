#' Appends a column to a data frame containing hex codes (ready
#' for usage in a plot) The values are determined by a user-specified column.
#' @param df Data frame containing the original data.
#' @param column A string containing the name of the column that will determine color.
#' @param output_name Name for the column in which the output will be stored. Defaults to "color"
#' @param min_color Color to be assigned to the row with the lowest value. Defaults to green.
#' @param max_color Color to be assigned to the row with the highest value. Defaults to red.
#' @param method Type of gradient. Allows both "rgb" and "hsv". Defaults to "hsv"
#' @param smooth_factor Parameter "smooth_factor" for "rgb_weighted_mean" call
#' @return A data frame identical to df, except for an extra column (whose name is given by output_name) containing hexcodes
#' @examples plot(mtcars$hp, mtcars$disp, col=df_color(mtcars, "wt")$color, pch=18, cex=1.5)
#' plot(mtcars$hp, mtcars$disp, col=df_color(mtcars, "wt", min_color="darkblue", max_color="darkred")$color, pch=18, cex=1.5)
#' plot(mtcars$hp, mtcars$disp, col=df_color(mtcars, "wt", min_color="darkblue", max_color="darkred", method="rgb", smooth_factor=1)$color, pch=18, cex=1.5)

df_color <- function(df, column, output_name="color", min_color="green",
                     max_color="red", method="hsv",
                     smooth_factor=2) {

  min_value <- min(df[,column])
  max_value <- max(df[,column])
  scaled_column <-(df[,column] - min_value)/(max_value - min_value)
  if(method=="rgb")
    df[,output_name] <- sapply(scaled_column, rgb_weighted_mean, col1=max_color,
                               col2=min_color, smooth_factor=smooth_factor, output="hex")
  if(method=="hsv")
    df[,output_name] <- sapply(scaled_column, hsv_weighted_mean, col1=max_color,
                               col2=min_color, output="hex")
  return(df)
}
