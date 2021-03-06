% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorblind.R
\name{colorblind}
\alias{colorblind}
\title{Returns the inputted color as perceivd by a colorblind person}
\usage{
colorblind(col, type = "deuteranopia", output = "hex")
}
\arguments{
\item{type}{Type of color blindness wanted to simulated. "normal", "protanopia", "protanomaly", "deuteranopia",
"deuteranomaly","tritanopia", "tritanomaly", "achromatopsia" and "achromatomally" are all supported. Defaults to "deuteranopia" (most common type.}

\item{output}{Output format. "hex" and "color" supported. Defaults to "hex"}

\item{...}{Input color. Hexcode, c(r,g,b), color
name as character and "color" class object are all supported.}
}
\value{
Generally, a hexcode representing the input's color_blindness. If a "color" class object is received as argument, the color_blindness will keep this format.
}
\description{
Returns the inputted color as perceivd by a colorblind person
}
\examples{
colorblind("red")
colorblind("#ff00ff")
colorblind(c(0,0,0))
colorblind("#ff00ff", type="tritanomaly")


}
