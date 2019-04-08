#' Appends a column named "color" to a data frame containing hex codes (ready
#' for usage in a plot) Three different columns from a data frame provide amounts for red, green
#' and blue light, which combine into a color for each observation, thus allowing the user to
#' represent simultaneaously up to three columns using color.
#' Darker/lighter colors represent simultaneous low/high values for all values.
#' Red/green/blue-ish colors represent high values in column1/2/3, but low ones in the remaining.
#' Cyan/magenta/yellow-ish colors represent low values in column1/2/3, but high ones in the remaining.
#' @param df Data frame containing the original data.
#' @param column1 A string containing the name of the column that will determine the output's amount of red light
#' @param column2 A string containing the name of the column that will determine the output's amount of green light.
#' @param column3 A string containing the name of the column that will determine the output's amounto of blue light.
#' @param output_name Name for the column in which the output will be stored. Defaults to "color"
#' @param avoid_white When set to TRUE, corrects colors that would be hard to distinguish in a white background.
#' @return A data frame identical to df, except for an extra column (whose name is given by output_name) containing hexcodes.
#' @examples plot(mtcars$hp, mtcars$disp, col=df_color3(mtcars, "drat", "wt", "mpg")$color, pch=18, cex=1.5)
#' plot(mtcars$hp, mtcars$disp, col=df_color3(mtcars, "drat", "wt", "mpg", avoid_white=FALSE)$color, pch=18, cex=1.5)


df_color3 <- function(df, column1, column2, column3, output_name="color",
                       avoid_white=TRUE) {

  min_value1=min(df[,column1])
  max_value1=max(df[,column1])
  min_value2=min(df[,column2])
  max_value2=max(df[,column2])
  min_value3=min(df[,column3])
  max_value3=max(df[,column3])

  coef=1-avoid_white*0.15
  scaled_column1 <- coef*(df[,column1] - min_value1)/(max_value1 - min_value1)
  scaled_column2 <- coef*(df[,column2] - min_value2)/(max_value2 - min_value2)
  scaled_column3 <- coef*(df[,column3] - min_value3)/(max_value3 - min_value3)
  color1 <- sapply(scaled_column1, rgb_weighted_mean, col1="red",
                   col2="black", output="hex")
  color2 <- sapply(scaled_column2, rgb_weighted_mean, col1="green",
                   col2="black", output="hex")
  color3 <- sapply(scaled_column3, rgb_weighted_mean, col1="blue",
                   col2="black", output="hex")
  for(i in 1:length(color1)) {
    df[i,output_name] <- mix(color1[i], color2[i], color3[i], method="light", average=FALSE,
                             output="hex")
  }

  return(df)
}
