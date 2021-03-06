% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_color.R
\name{is_color}
\alias{is_color}
\title{Checks whether an object belongs to class "color"}
\usage{
is_color(x)
}
\arguments{
\item{x}{The object to be checked.}
}
\value{
A logical indicating whether x is a class "color" object.
}
\description{
Checks whether an object belongs to class "color"
}
\examples{
is_color(3)
is_color("red")
is_color(color("red"))
}
