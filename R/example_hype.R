#' Generate an Example Hypergraph
#'
#' Quickly generate an example hypergraph. Can be used for quickly testing and
#' trialing examples.
#'
#' @param oriented Logical value representing whether the example hypergraph
#'     should be oriented
#' @param directed Logical value representing whether the example hypergraph
#'     should be directed
#' @param vertex_weighted Logical value representing whether the example
#'     hypergraph should have vertex weights
#' @param edge_weighted Logical value representing whether the example
#'     hypergraph should have hyperedge weights
#' @param real_coef Logical value representing whether the example hypergraph
#'     should have real coefficients relating vertices to hyperedges
#'
#' @return An example hypergraph with the given properties
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' h2 <- example_hype(oriented = TRUE)
#' h3 <- example_hype(oriented = TRUE, directed = TRUE)
#' h4 <- example_hype(oriented = TRUE, directed = TRUE, real_coef = TRUE)
example_hype <- function(oriented = FALSE,
                         directed = FALSE,
                         vertex_weighted = FALSE,
                         edge_weighted = FALSE,
                         real_coef = FALSE) {

  # Generating error if user asks for a directed but unoriented hypergraph
  if (!oriented & directed) {
    stop("\n \u2716 A hypergraph can't be directed but unoriented")
  }

  # setting the number of vertices
  numv <- 4

  # Setting the hyperedge list
  if (oriented) {
    elist <- list(
      list(
        c(1, 2),
        c(3, 4)
      ),
      list(
        c(2, 3, 4),
        c(1, 2)
      )
    )
    if (directed) {
      names(elist[[1]]) <- c("from", "to")
      names(elist[[2]]) <- c("from", "to")
    }
  } else {
    elist <- list(
      c(1, 2, 3),
      c(2, 3, 4)
    )
  }

  # Setting the vertex and hyperedge names
  vnames <- paste0("v", 1:4)
  enames <- paste0("h", 1:2)
  names(elist) <- enames

  # Setting vertex and hyperedge weights
  if (vertex_weighted & edge_weighted) {
    vweights <- 1:4
    eweights <- 1:2
    names(vweights) <- vnames
    names(eweights) <- enames
  } else if (vertex_weighted & !edge_weighted) {
    vweights <- 1:4
    eweights <- rep(1, 2)
    names(vweights) <- vnames
    names(eweights) <- enames
  } else if (!vertex_weighted & edge_weighted) {
    vweights <- rep(1, 4)
    eweights <- 1:2
    names(vweights) <- vnames
    names(eweights) <- enames
  } else {
    vweights <- NULL
    eweights <- NULL
  }

  # Setting real coefficients
  if (real_coef) {
    if (oriented) {
      inc_mat1 <- matrix(c(1, 2, 0, 0, 0, 2, 3, 4), ncol = 2, nrow = 4)
      inc_mat2 <- matrix(c(0, 0, 3, 4, 1, 2, 0, 0), ncol = 2, nrow = 4)

      rownames(inc_mat1) <- vnames
      colnames(inc_mat1) <- enames
      rownames(inc_mat2) <- vnames
      colnames(inc_mat2) <- enames

      inc_mat <- list(inc_mat1, inc_mat2)
      if (directed) {
        names(inc_mat) <- c("from", "to")
      }
    } else {
      inc_mat <- matrix(c(1, 2, 3, 0, 0, 2, 3, 4), ncol = 2, nrow = 4)

      rownames(inc_mat) <- vnames
      colnames(inc_mat) <- enames
    }
  } else {
    inc_mat <- NULL
  }

  # Generating and returning hypergraph
  Hypergraph$new(
    numv = numv,
    elist = elist,
    vnames = vnames,
    vweights = vweights,
    enames = enames,
    eweights = eweights,
    weighted = (vertex_weighted | edge_weighted),
    oriented = oriented,
    directed = directed,
    real_coef = real_coef,
    inc_mat = inc_mat
  )
}
