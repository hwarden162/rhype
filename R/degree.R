#' Find the Degree of Vertices in a Hypergraph
#'
#' The degree of a vertex is a way of expressing how many connections there
#' are from a vertex to the rest of the hypergraph. The current version of
#' rhype has three methods for computing degree.
#'
#' `"vertex"` counts the number of ways it is possible to move to another
#' vertex. If there are multiple hyperedges connecting two vertices, then each
#' of these hyperedges will be counted as a new way to move between these two
#' vertices. For weighted hypergraphs or hypergraphs with real coefficients,
#' the strength of connection between two vertices is a functions of the weights
#' and real coefficients.
#'
#' `"vertex_simple"` just counts the number of vertices it is possible to reach
#' in one step from the given vertex, no matter how many hyperedges connect them.
#'
#' `"hyperedge"` represents the strength with which a vertex connects with
#' itself through the hyperedges it is a member of. This is taken from the work
#' of Jurgen Jost and Raffaella Mulas \doi{10.1016/j.aim.2019.05.025}. For
#' unweighted hypergraphs without real coefficients this is equivalent to
#' `"hyperedge_simple"`.
#'
#' `"hyperedge_simple"` just counts the number of hyperedges a vertex is a
#' member of.
#'
#' @param hype A hypergraph object
#' @param method The method for calculating degree. Out of `"vertex"`,
#' `"vertex_simple"`, `"hyperedge"` and `"hyperedge_simple"`
#'
#' @return A vector representing the degree of each vertex with respect to the
#' given method.
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' degree(h1)
degree <- function(hype, method = NA) {
  # Checking the hyeprgraph is not oriented
  if (hype$get_oriented()) {
    stop("\n \u2716 Degree not yet supported for oriented hypergraphs")
  }
  if (is.na(method)) {
    warning("Degree method not chosen, using 'hyperedge' by default")
    method <- "hyperedge"
  }

  # Checking method and computing the corresponding degree
  if (method == "vertex") {
    # Finding the adjacency matrix
    adj_mat <- adjacency_matrix(hype, normalise = TRUE, self_adj = FALSE, as_matrix = FALSE)
    # Row summing the adjacency matrix
    return(
      apply(adj_mat, 1, sum)
    )
  } else if (method == "vertex_simple") {
    # Finding the normalised adjacency matrix
    adj_mat <- adjacency_matrix(hype, normalise = TRUE, self_adj = FALSE, as_matrix = FALSE)
    # Row summing the normalised adjacency matrix
    return(apply(adj_mat, 1, sum))
  } else if (method == "hyperedge") {
    # Finding the adjacency matrix with self adjacency
    adj_mat <- adjacency_matrix(hype, normalise = FALSE, self_adj = TRUE, as_matrix = FALSE)
    # Returning the self adjacency
    return(Matrix::diag(adj_mat))
  } else if (method == "hyperedge_simple") {
    # Finding the incidence matrix
    inc_mat <- incidence_matrix(hype)
    # Normalising the incidence matrix
    inc_mat <- matrix(
      as.numeric(inc_mat != 0),
      nrow = dim(inc_mat)[1],
      dimnames = dimnames(inc_mat)
    )
    # Row summing the incidence matrix and returning this value
    return(apply(inc_mat, 1, sum))
  }
}

#' Find The Maximum Degree Of A Hypergraph
#'
#' The degree of a vertex is a way of expressing how many connections there
#' are from a vertex to the rest of the hypergraph. See the `degree` help
#' documentation for more details.
#'
#' @param hype A hypergraph object.
#' @param method The method by which to calculate the degree, see `degree` help
#' documentation for more information.
#'
#' @return The maximum value for the chosen degree among vertices of the hypergraph.
#' @export
#'
#' @examples
#' h <- example_hype()
#' max_degree(h)
max_degree <- function(hype, method = NA) {
  deg <- degree(hype, method = method)
  return(max(deg))
}

#' Find The Minimum Degree Of A Hypergraph
#'
#' The degree of a vertex is a way of expressing how many connections there
#' are from a vertex to the rest of the hypergraph. See the `degree` help
#' documentation for more details.
#'
#' @param hype A hypergraph object.
#' @param method The method by which to calculate the degree, see `degree` help
#' documentation for more information.
#'
#' @return The maximum value for the chosen degree among vertices of the hypergraph.
#' @export
#'
#' @examples
#' h <- example_hype()
#' min_degree(h)
min_degree <- function(hype, method = NA) {
  deg <- degree(hype, method = method)
  return(min(deg))
}

#' Find The Cardinality Of Hyperedges In A Hypergraph
#'
#' The cardinality of a hyperedge is the number of vertices that it contains.
#'
#' @param hype A hypergraph object.
#'
#' @return A vector of the cardinality of the hyperedges.
#' @export
#'
#' @examples
#' hype <- example_hype()
#' cardinality(hype)
cardinality <- function(hype) {
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is not yet available for oriented hypergraphs")
  }
  if (hype$get_real_coef()) {
    stop("\n \2716 This function is not yet supported for hypergraphs with real coefficients")
  }

  inc_mat <- incidence_matrix(hype)
  card <- apply(inc_mat, 2, sum)
  return(card)
}

#' Find The Maximum Cardinality Of A Hyperedge In A Hypergraph
#'
#' The cardinality of a hyperedge is the number of vertices that it contains.
#'
#' @param hype A hypergraph object.
#'
#' @return The value of the maximum cardinality of a hyperedge in the hypergraph.
#' @export
#'
#' @examples
#' hype <- example_hype()
#' max_cardinality(hype)
max_cardinality <- function(hype) {
  card <- cardinality(hype)
  return(max(card))
}

#' Find The Minimum Cardinality Of A Hyperedge In A Hypergraph
#'
#' The cardinality of a hyperedge is the number of vertices that it contains.
#'
#' @param hype A hypergraph object.
#'
#' @return The value of the minimum cardinality of a hyperedge in the hypergraph.
#' @export
#'
#' @examples
#' hype <- example_hype()
#' min_cardinality(hype)
min_cardinality <- function(hype) {
  card <- cardinality(hype)
  return(min(card))
}
