#' Get Hyperedge List
#'
#' Take a hypergraph object and return its hyperedge list.
#'
#' @param hype A hypergraph object
#'
#' @return A hyperedge list. See main documentation for more details on its
#'     structure
#' @export
#'
#' @examples
#' h <- example_hype()
#' hyperedge_list(h)
hyperedge_list <- function(hype) {
  hype$get_elist()
}

#' Get Vertex Names
#'
#' Takes a hypergraph object and returns the names of its vertices.
#'
#' @param hype A hypergraph object.
#'
#' @return A vector of strings of vertex names
#' @export
#'
#' @examples
#' h <- example_hype()
#' vertex_names(h)
vertex_names <- function(hype) {
  hype$get_vnames()
}

#' Get Vertex Weights
#'
#' Takes a hypergraph object and returns the weights associated with its
#' vertices.
#'
#' @param hype A hypergraph object.
#'
#' @return A vector of weights associated with each vertex. If the hypergraph
#' has no weights associated with its vertices it will return `NULL` instead.
#' @export
#'
#' @examples
#' h <- example_hype()
#' vertex_weights(h)
vertex_weights <- function(hype) {
  hype$get_vweights()
}

#' Get Hyperedge Names
#'
#' Takes a hypergraph object and returns the names of the hyperedges.
#'
#' @param hype A hypergraph object.
#'
#' @return A vector of strings representing the names of the the hyperedges. If
#'     the hyperedges have no names assocaited with them it will return `NULL`
#'     instead.
#' @export
#'
#' @examples
#' h <- example_hype()
#' hyperedge_names(h)
hyperedge_names <- function(hype) {
  hype$get_enames()
}

#' Get Hyperedge Weights
#'
#' Takes a hypergraph object and returns the weights associated with each
#' hyperedge
#'
#' @param hype A hypergraph object.
#'
#' @return A vector of weights asssociated with the hyperedges. If the are no
#'     weights assicated with the hyperedges then `NULL` is returned instead.
#' @export
#'
#' @examples
#' h <- example_hype()
#' hyperedge_weights(h)
hyperedge_weights <- function(hype) {
  hype$get_eweights()
}

#' Is a Hypergraph Weighted
#'
#' Takes a hypergraph object and returns whether a hypergraph has weights
#' associated with its vertices or hyperedges.
#'
#' @param hype A hypergraph object.
#'
#' @return A logical value indicating whether the hypergraph has weights
#'     associated with its vertices or hyperedges.
#' @export
#'
#' @examples
#' h <- example_hype()
#' is_weighted(h)
is_weighted <- function(hype) {
  hype$get_weighted()
}

#' Is a Hypergraph Oriented
#'
#' Takes a hypergraph object and returns whether the hyperedges are oriented.
#'
#' @param hype A hypergraph object.
#'
#' @return A logical value indicating whether the hyperedges are oriented.
#' @export
#'
#' @examples
#' h <- example_hype()
#' is_oriented(h)
is_oriented <- function(hype) {
  hype$get_oriented()
}

#' Is a Hypergraph Directed
#'
#' Takes a hypergraph object and returns whether the hyperedges are directed.
#'
#' @param hype A hyeprgraph object.
#'
#' @return A logical value indicating whether the hyperedges are directed.
#' @export
#'
#' @examples
#' h <- example_hype()
#' is_directed(h)
is_directed <- function(hype) {
  hype$get_directed()
}

#' Does a Hypergraph Have Real Coefficients
#'
#' Takes a hypergraph object and returns whether there are real coefficients
#' associating vertices to hyperedges.
#'
#' @param hype A hypergraph object.
#'
#' @return A logical value indicating whether there are real cofficients
#' associating vertices to hyperedges.
#' @export
#'
#' @examples
#' h <- example_hype()
#' has_real_coef(h)
has_real_coef <- function(hype) {
  hype$get_real_coef()
}
