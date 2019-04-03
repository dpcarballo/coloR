% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_color.R
\name{df_color}
\alias{df_color}
\title{Appends a column named "color" to a data frame containing hex codes (ready
for usage in a plot) The values are determined by a user-specified column.}
\usage{
df_color(df, column, output_name = "color", min_color = "green",
  max_color = "red", min_value = min(df[, column]),
  max_value = max(df[, column]), method = "hsv")
}
\arguments{
\item{df}{Data frame containing the original data.}

\item{column}{A string containing the name of the column that will determine color.}

\item{output_name}{Name for the column in which the output will be stored. Defaults to "color"}

\item{min_color}{Color to be assigned to the row with the lowest value. Defaults to green.}

\item{max_color}{Color to be assigned to the row with the highest value. Defaults to red.}

\item{min_value}{Allows the user to customize the value that gets min_color assigned. Defaults to the minium of all observations.}

\item{max_value}{Allows the user to customize the value that gets max_color assigned. Defaults to the maximum of all observations.}

\item{method}{Type of gradient. Allows both "rgb" and "hsv". Defaults to "hsv"}
}
\description{
Appends a column named "color" to a data frame containing hex codes (ready
for usage in a plot) The values are determined by a user-specified column.
}