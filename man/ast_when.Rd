% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ast-classes.R
\name{ast_when}
\alias{ast_when}
\title{Construct a conditional expression node for multimod AST}
\usage{
ast_when(condition, then, otherwise = NULL, ...)
}
\arguments{
\item{condition}{The condition to check, must be an AST node.}

\item{then}{The expression to evaluate if the condition is true
(usually the left-hand side).}

\item{otherwise}{Optional expression to evaluate if the condition is false.
This part is not available in GAMS \code{$} statements, but is useful for other
languages.}
}
\value{
An object of class \code{ast} and subclass \code{when}.
}
\description{
This function constructs a conditional expression node of type \code{"when"},
representing GAMS-style conditional terms using the \code{$} operator,
\verb{if ... else ...} statements in Julia/JuMP,  \verb{if ... in ...} in Python/Pyomo,
etc.
}
\examples{
ast_when(
  condition = ast_symbol("i_active(i)"),
  then = ast_variable("x", c("i"))
)
}
