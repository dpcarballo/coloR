#' Appends a column named "color" to a data frame containing hex codes (ready
#' for usage in a plot) The values are determined by mixing two gradients that depend on two columns
#' specified by the user.Therefore, the output is able to represent the value of two columns together.
#' Simultaneous low values for both columns return darker colors.
#' @param df Data frame containing the original data.
#' @param column1 A string containing the name of the column that will determine color's first gradient.
#' @param column2 A string containing the name of the column that will determine color's second gradient.
#' @param output_name Name for the column in which the output will be stored. Defaults to "color"
#' @param max_color1 Color to be assigned to the row with the highest value in column1. Defaults to red
#' @param max_color2 Color to be assigned to the row with the highest value in column2. Defaults to green.
#' @param min_value1 Allows the user to customize the value for column1 that gets no light assigned. Defaults to the minium of all observations.
#' @param max_value1 Allows the user to customize the value for column1 that gets maximum light assigned. Defaults to the maximum of all observations.
#' @param min_value2 Allows the user to customize the value for column1 that gets no light assigned. Defaults to the minium of all observations.
#' @param max_value2 Allows the user to customize the value for column1 that gets maximum light assigned. Defaults to the maximum of all observations.

df_color2 <- function(df, column1, column2, output_name="color",
                      max_color1="red", max_color2="green",
                      min_value1=min(df[,column1]), max_value1=max(df[,column1]),
                      min_value2=min(df[,column2]), max_value2=max(df[,column2])) {
  
  scaled_column1 <- (df[,column1] - min_value1)/(max_value1 - min_value1)
  scaled_column2 <- (df[,column2] - min_value2)/(max_value2 - min_value2)
  color1 <- sapply(scaled_column1, rgb_weighted_mean, col1=max_color1,
                   col2="black", output="hex")
  color2 <- sapply(scaled_column2, rgb_weighted_mean, col1=max_color2,
                   col2="black", output="hex")
  for(i in 1:length(color1)) {
    df[i,output_name] <- mix(color1[i], color2[i], method="light", average=FALSE,
                             output="hex")
  }
  
  return(df)
}