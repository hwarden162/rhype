% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resamples.R
\name{jackknife_hype}
\alias{jackknife_hype}
\title{Jackknife A Hypergraph}
\usage{
jackknife_hype(hype, n = 1, method = "both")
}
\arguments{
\item{hype}{A hypergraph object.}

\item{n}{The number of hypergraphs to create.}

\item{method}{The method to use to jackknife the hypergraphs.}
}
\value{
A list of jackknifed hypergraphs.
}
\description{
Jackknifing is a resampling technique similar to bootstrapping, where many
resamples are taken, each time leaving out one observation. For the abstraction
to hypergraphs, the \code{"vertex"} method recreates the hypergraph leaving out one
vertex, the \code{"hyperedge"} method recreates the hypergraph leaving out one hyperedge
and the \code{"both"} method leaves out one of each.
}
\examples{
hype <- example_hype()
resamples <- jackknife_hype(hype, n = 5)
lapply(resamples, incidence_matrix)
}
