% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_special}
\alias{is_special}
\title{Check if a character is a special character}
\usage{
is_special(ch)
}
\arguments{
\item{ch}{A character string to check.}
}
\value{
A logical value indicating whether the character is a special character.
}
\description{
Check if a character is a special character
}
\examples{
is_special("!") # TRUE
is_special("a") # FALSE
is_special("1") # FALSE
is_special(1) # FALSE
is_special("#") # TRUE
}
