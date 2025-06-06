% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{resolve_full_name}
\alias{resolve_full_name}
\title{Resolve alias to full set name}
\usage{
resolve_full_name(
  alias,
  base = base_aliases,
  extended = extended_aliases,
  default = NULL
)
}
\arguments{
\item{alias}{A character vector of alias names (e.g., "r", "yp").}

\item{base}{Named list of base aliases.}

\item{extended}{Named list of extended aliases.}

\item{default}{Fallback if alias not found.}
}
\value{
Character vector of full names.
}
\description{
Resolve alias to full set name
}
