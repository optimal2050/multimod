% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{depth}
\alias{depth}
\title{Get the depth of a nested list structure (AST, multimod, and other objects)}
\usage{
depth(x)
}
\arguments{
\item{x}{A list or nested list structure.}
}
\value{
An integer representing the depth of the list.
}
\description{
This function calculates the depth of a nested list structure.
Wrapper for \code{purrr::pluck_depth}.
}
