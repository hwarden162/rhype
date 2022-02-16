#' Find the Spectra of a Hypergraph
#'
#' @param hype A hypergraph object
#' @param matrix The matrix to calculate the spectra with respect to. Out of
#' `"laplacian"`, `"adjacency"`, `"vert_norm_lap_mat"` and `"hype_norm_lap_mat"`
#' @param n The number of eigenvalues or eigenvectors to calculate. If left empty or
#' as `NULL` all will be calculated.
#'
#' @return The eigen decomposition of the given matrix of the given hypergraph
#' @export
#'
#' @examples
#' h <- example_hype()
#' spectra(h)
spectra <- function(hype, matrix = "laplacian", n = NULL) {
  # Finding the eigen decomposition of the relevant matrix
  if (matrix == "laplacian") {
    if (is.null(n)) {
      n <- hype$get_numv()
    }
    lap_mat <- laplacian_matrix(hype)
    if (n == hype$get_numv()) {
      return(
        eigen(as.matrix(lap_mat))
      )
    } else {
      return(
        RSpectra::eigs(
          lap_mat,
          k = n
        )
      )
    }
  } else if (matrix == "adjacency") {
    if (is.null(n)) {
      n <- hype$get_numv()
    }
    adj_mat <- adjacency_matrix(hype, normalise = FALSE, self_adj = TRUE)
    if (n == hype$get_numv()) {
      return(
        eigen(as.matrix(adj_mat))
      )
    } else {
      return(
        RSpectra::eigs(
          adj_mat,
          k = n
        )
      )
    }
  } else if (matrix == "vert_norm_lap_mat") {
    if (is.null(n)) {
      n <- hype$get_numv()
    }
    lap_mat <- vert_norm_lap_mat(hype)
    if (n == hype$get_numv()) {
      return(
        eigen(as.matrix(lap_mat))
      )
    } else {
      return(
        RSpectra::eigs(
          lap_mat,
          k = n
        )
      )
    }
  } else if (matrix == "hype_norm_lap_mat") {
    if (is.null(n)) {
      n <- length(hype$get_elist())
    }
    lap_mat <- hype_norm_lap_mat(hype)
    if (n == length(hype$get_elist())) {
      return(
        eigen(as.matrix(lap_mat))
      )
    } else {
      return(
        RSpectra::eigs(
          lap_mat,
          k = n
        )
      )
    }
  }
}

#' Find the Spectral Distance Between Two Hypergraphs
#'
#' @param hype1 A hypergraph object
#' @param hype2 A hypergraph object
#' @param matrix The matrix to calculate the spectral distance with respect to.
#' Out of `"laplacian"`, `"adjacency"`, `"vert_norm_lap_mat"` and
#' `"hype_norm_lap_mat"`
#'
#' @return A number representing the spectral distance between the two hypergraphs
#' with respect to the given matrix
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' h2 <- example_hype()
#' spectral_distance(h1, h2)
spectral_distance <- function(hype1, hype2, matrix = "laplacian") {
  # Finding the eigenvalues of both hypergraphs
  eigen1 <- spectra(hype1, matrix)$values
  eigen2 <- spectra(hype2, matrix)$values

  # Fiding the spectral distance
  spec_dist <- sum(abs(eigen1 - eigen2))
  # Returning the spectral distance
  return(spec_dist)
}

#' Find the Spectral Distance From the Fully Disconnected Hypergraph
#'
#' @param hype A hypergraph object
#' @param matrix The matrix to calculate the spectra with respect to. Out of
#' `"vert_norm_lap_mat"` and `"hype_norm_lap_mat"`
#'
#' @return The spectral distance from the disconnected hypergraph
#' @export
#'
#' @examples
#' h <- example_hype()
#' spectral_distance_disc(h)
spectral_distance_disc <- function(hype, matrix = "vert_norm_lap_mat") {
  # Checking the matrix is of a valid type
  if ((matrix != "vert_norm_lap_mat") & (matrix != "hype_norm_lap_mat")) {
    stop("\n \u2716 Function only valid for vertex/hyperedge normalised laplacians")
  }

  # Finding the spectra of the given hypergraph
  eigen1 <- spectra(hype, matrix)$values
  # Finding the spectra of the respective disconnected hypergraph
  eigen2 <- rep(1, length(eigen1))

  # Finding the spectral distance between the hypergraphs
  spec_dist <- sum(abs(eigen1 - eigen2))
  # Returning the spectral distance
  return(spec_dist)
}
