% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ast-classes.R
\name{node_hash}
\alias{node_hash}
\title{Generate a stable hash for an AST or multimod object (excluding internal hash fields)}
\usage{
node_hash(node, algo = NULL)
}
\arguments{
\item{node}{An \code{ast} object.}

\item{algo}{Hashing algorithm. If NULL, uses \code{options("multimod.hash_algo")}.}
}
\value{
A character hash (e.g., "42fca0dd").
}
\description{
Generate a stable hash for an AST or multimod object (excluding internal hash fields)
}
\examples{
ast <- ast_expression("+", ast_variable("x"), ast_constant(5))
hash <- node_hash(ast)
}
