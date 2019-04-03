#' Appends a column named "color" to a data frame containing hex codes (ready
#' for usage in a plot) The values are determined by a user-specified column.
#' @param df Data frame containing the original data.
#' @param column A string containing the name of the column that will determine color.
#' @param output_name Name for the column in which the output will be stored. Defaults to "color"
#' @param min_color Color to be assigned to the row with the lowest value. Defaults to green.
#' @param max_color Color to be assigned to the row with the highest value. Defaults to red.
#' @param min_value Allows the user to customize the value that gets min_color assigned. Defaults to the minium of all observations.
#' @param max_value Allows the user to customize the value that gets max_color assigned. Defaults to the maximum of all observations.
#' @param method Type of gradient. Allows both "rgb" and "hsv". Defaults to "hsv"
df_color <- function(df, column, output_name="color", min_color="green", 
                     max_color="red",min_value=min(df[,column]), 
                     max_value=max(df[,column]), method="hsv") {
  
  scaled_column <-(df[,column] - min_value)/(max_value - min_value)
  if(method=="rgb")
    df[,output_name] <- sapply(scaled_column, rgb_weighted_mean, col1=max_color,
                               col2=min_color, output="hex")
  if(method=="hsv")
    df[,output_name] <- sapply(scaled_column, hsv_weighted_mean, col1=max_color,
                               col2=min_color, output="hex")
  return(df)
}