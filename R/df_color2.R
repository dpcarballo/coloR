#' Appends a column named "color" to a data frame containing hex codes (ready
#' for usage in a plot) The values are determined by mixing two gradients that depend on two columns
#' specified by the user.Therefore, the output is able to represent the value of two columns together.
#' With "rgb" method, simultaneous low values for both columns return darker colors.
#' With "hsv" method and "value" as second_parameter, darker points represent low values for the second variable. The first variable is represented by a blue-green-red gradient
#' With "hsv" method and "saturation" as second_parameter, attenuated colors represent low values for the second variable. The first variable is represented by a blue-green-red gradient
#' @param df Data frame containing the original data.
#' @param column1 A string containing the name of the column that will determine color's first gradient.
#' @param column2 A string containing the name of the column that will determine color's second gradient.
#' @param output_name Name for the column in which the output will be stored. Defaults to "color"
#' @param max_color1 In case "rgb" method is used, color to be assigned to the row with the highest value in column1. Defaults to red
#' @param max_color2 In case "rgb" method is used, color to be assigned to the row with the highest value in column2. Defaults to green.
#' @param method Type of gradient. Allows both "rgb" and "hsv". Defaults to "rgb"
#' @param second_param In case "hsv" method is used, choose between "saturation" and "value" to be used as second parameter.
#' @param min_saturation In case "hsv" method is used, sets minimum allowed value for "saturation". Data points with too low saturation may not appear visible. Defaults to 0.2
#' @param min_value In case "hsv" method is used, sets minimum allowed value for "value". Data points with too low values may appear black regardless of the "hue" parameter. Defaults to 0.4
#' @return A data frame identical to df, except for an extra column (whose name is given by output_name) containing hexcodes.
#' @examples
#' plot(mtcars$hp, mtcars$disp, col=df_color2(mtcars, "drat", "wt",method="rgb")$color, pch=18, cex=1.5)
#' plot(mtcars$hp, mtcars$disp, col=df_color2(mtcars, "drat", "wt",method="rgb", max_color1="green", max_color2="blue")$color, pch=18, cex=1.5)
#' plot(mtcars$hp, mtcars$disp, col=df_color2(mtcars, "drat", "wt", second_param = "saturation", method="hsv")$color, pch=18, cex=1.5)
#' plot(mtcars$hp, mtcars$disp, col=df_color2(mtcars, "drat", "wt", second_param ="value", method="hsv")$color, pch=18, cex=1.5)


df_color2 <- function(df, column1, column2, output_name="color",
                      max_color1="red", max_color2="green",
                      method="rgb", second_param = "saturation",
                      min_saturation=0.2, min_value=0.4) {

  min_value1=min(df[,column1])
  max_value1=max(df[,column1])
  min_value2=min(df[,column2])
  max_value2=max(df[,column2])

  scaled_column1 <- (df[,column1] - min_value1)/(max_value1 - min_value1)
  scaled_column2 <- (df[,column2] - min_value2)/(max_value2 - min_value2)
  if(method=="rgb") {
    color1 <- sapply(scaled_column1, rgb_weighted_mean, col1=max_color1,
                     col2="black", output="hex", smooth_factor=2)
    color2 <- sapply(scaled_column2, rgb_weighted_mean, col1=max_color2,
                     col2="black", output="hex", smooth_factor=2)
    for(i in 1:length(color1)) {
      df[i,output_name] <- mix(color1[i], color2[i], method="light", average=FALSE,
                               output="hex")
    }
  } else if(method=="hsv") {
    if(second_param=="saturation") {
      for(i in 1:length(scaled_column1)) {
        df[i,output_name] <- hex(hsv_color(scaled_column1[i],
                                           min_saturation+(1-min_saturation)*scaled_column2[i],
                                           1,
                                           hmax=1,
                                           vmax=1))
      }
    } else if(second_param=="value") {
      for(i in 1:length(scaled_column1)) {
        df[i,output_name] <- hex(hsv_color(scaled_column1[i], 1, min_value+(1-min_value)*scaled_column2[i], hmax=1, vmax=1))
      }
    }
  }


  return(df)
}
