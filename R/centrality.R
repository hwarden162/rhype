#' Calculate The Eigenvector Centrality Of A Hypergraph
#'
#' To calculate the eigenvector centrality of a hypergraph, each vertex is assigned
#' a value that is proportional to the sum of the value of its neighbours.
#'
#' @param hype A hypergraph object
#'
#' @return A vector of values representing the eigenvector centrality of each node
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' eigenvector_centrality(h1)
eigenvector_centrality <- function(hype) {
  # Getting the eigenvectors of the adjacency matrix
  e_vecs <- spectra(hype, matrix = "adjacency")$vectors
  # Finding the largest eigenvector
  cent <- e_vecs[,1]
  # Flipping the eigenvector if it is pointing in a negative direction
  if (any(cent < 0)) {
    cent <- -cent
  }
  # Setting the names of the centrality vector
  names(cent) <- hype$get_vnames()
  # Returning the centrality vector
  return(cent)
}

#' Calculate The Eigenvector Centrality Scaling Factor Of A Hypergraph
#'
#' To calculate the eigenvector centrality of a hypergraph, each vertex is assigned
#' a value that is proportional to the sum of the value of its neighbours. This
#' function gives the scaling factor relating the value of each node to the sum of
#' the value of its neighbours.
#'
#' @param hype A hypergraph object
#'
#' @return A number representing the scaling factor relating the value of each node
#' to the sum of the value of its neighbours
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' eigenvector_centrality_factor(h1)
eigenvector_centrality_factor <- function(hype) {
  # Returning the largest eigenvalue of the adjacency matrix
  spectra(hype, matrix = "adjacency")$values[1]
}
