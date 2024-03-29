% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getters_setters.R
\name{hype_order}
\alias{hype_order}
\title{Get The Order Of A Hypergraph}
\usage{
hype_order(hype)
}
\arguments{
\item{hype}{A hypergraph object}
}
\value{
A number representing the number of vertices in the hypergraph
}
\description{
The order of a hypergraph is the number of vertices it has
}
\examples{
hype <- example_hype()
hype_order(hype)
}
