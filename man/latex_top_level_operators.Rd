% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_latex.R
\name{latex_top_level_operators}
\alias{latex_top_level_operators}
\title{Identify top-level LaTeX operators in an expression}
\usage{
latex_top_level_operators(
  latex_str,
  operators = c("+", "-", "\\\\\\\\cdot", "\\\\\\\\div", "=")
)
}
\arguments{
\item{latex_str}{A LaTeX math string}

\item{operators}{Vector of operators to detect at top level}
}
\value{
Data frame of matched operators and positions
}
\description{
Identify top-level LaTeX operators in an expression
}
