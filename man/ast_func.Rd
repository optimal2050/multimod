% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ast-classes.R
\name{ast_func}
\alias{ast_func}
\title{Create a function AST node}
\usage{
ast_func(name, value, index = NULL, ...)
}
\arguments{
\item{name}{A character string representing the function name.}

\item{value}{An AST node representing the function body or expression.}

\item{index}{An AST node (typically dims or when) defining the index set.}
}
\value{
An \code{ast} object of class \code{function}.
}
\description{
Constructs an abstract syntax tree (AST) node representing a function.
}
