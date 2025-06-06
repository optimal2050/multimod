% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_gams_expression}
\alias{is_gams_expression}
\title{Check if a string is a compound expression}
\usage{
is_gams_expression(
  s,
  ops = c("+", "-", "*", "/", "^", "=", "==", "<", "<=", ">", ">=", "=e=", "=l=", "=g=",
    "=le=", "=ge=", "and", "or", "not")
)
}
\arguments{
\item{s}{Character string (GAMS-like expression)}

\item{ops}{Character vector of known operators (defaults: arithmetic, logical, relational)}
}
\value{
Logical: TRUE if expression, FALSE if atomic
}
\description{
Check if a string is a compound expression
}
\examples{
is_gams_expression("x + y") # TRUE
is_gams_expression("x") # FALSE

}
