% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_vizNetwork.R
\name{get_network_data}
\alias{get_network_data}
\title{Get network data for a multimod equation}
\usage{
get_network_data(x, alias_map = NULL, show_dims = TRUE)
}
\arguments{
\item{alias_map}{A named list for alias mapping (optional)}

\item{show_dims}{Logical, whether to show dimensions in the label (default: FALSE)}

\item{eq}{A multimod equation object}
}
\value{
A list with two data frames: nodes and edges to be used with
visNetwork or similar packages for visualization.
}
\description{
Get network data for a multimod equation
}
