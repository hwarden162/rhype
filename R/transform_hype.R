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
#' dual_hype(hype)
dual_hype <- function(hype) {
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
  new_inc_mat <- inc_mat[,hyperedges, drop = FALSE]
  hype_from_inc_mat(new_inc_mat)
}
