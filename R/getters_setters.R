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

#' Get The Order Of A Hypergraph
#'
#' The order of a hypergraph is the number of vertices it has
#'
#' @param hype A hypergraph object
#'
#' @return A number representing the number of vertices in the hypergraph
#' @export
#'
#' @examples
#' hype <- example_hype()
#' hype_order(hype)
hype_order <- function(hype) {
  hype$get_numv()
}

#' Get The Size Of A Hypergraph
#'
#' The size of a hypergraph is the number of hyperedges it contains
#'
#' @param hype A hypergraph object
#'
#' @return A number representing the number of hyperedges in a hypergraph
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' hype_size(h1)
hype_size <- function(hype) {
  length(hype$get_elist())
}

#' Get The Dual Of A Hypergraph
#'
#' The dual of a hypergraph has a vertex for each original hyperedge and a hyperedge
#' for each original vertex. A vertex is a member of a hyperedge if the original
#' hyperedge has the original vertex as a member.
#'
#' @param hype A hypergraph object.
#'
#' @return A hypergraph object representing the dual of the hypergraph.
#' @export
#'
#' @examples
#' hype <- example_hype()
#' hype_dual(hype)
hype_dual <- function(hype) {
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is currently not available for oriented hypergraphs")
  }
  if (hype$get_weighted()) {
    stop("\n \u2716 This function is currently not available for weighted hypergraphs")
  }
  inc_mat <- incidence_matrix(hype)
  ret_hype <- hype_from_inc_mat(t(inc_mat))
  return(ret_hype)
}

#' Generate A Partial Hypergraph
#'
#' A partial hypergraph can be induced from a set of hyperedges. The partial
#' hypergraph has all of the original vertices, but only the hyperedges used
#' to induce it.
#'
#' @param hype A hypergraph object.
#' @param hyperedges A vector of the names of the hyperedges to be used to
#' induce the partial hypergraph.
#'
#' @return A hypergraph object of the partial hypergraph.
#' @export
#'
#' @examples
#' hype <- example_hype()
#' partial_hype(hype, c("h1", "h2"))
partial_hype <- function(hype, hyperedges) {
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is currently not available for oriented hypergraphs")
  }
  if (hype$get_weighted()) {
    stop("\n \u2716 This function is currently not available for weighted hypergraphs")
  }
  inc_mat <- incidence_matrix(hype)
  new_inc_mat <- inc_mat[,hyperedges]
  hype_from_inc_mat(new_inc_mat)
}
