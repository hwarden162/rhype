#' Pseudo-Invert a Vector
#'
#' Pseudoinversion is where a vector has each non-zero element inverted and each zero
#' element remains untouched. This is useful for pseudoinverting matrices that only have
#' non-zero entries on the leading diagonal.
#'
#' @param vec A vector of numbers
#'
#' @return A vector of pseudo-inverted numbers
pseudo_invert <- function(vec) {
  # Cycle through the vector
  for (i in 1:length(vec)) {
    # Invert the entry if it is non-zero
    if (vec[i] != 0) {
      vec[i] <- 1 / vec[i]
    }
  }
  return(vec)
}

#' Find the Incidence Matrix of a Hypergraph
#'
#' An incidence matrix has rows indexed by vertices and columns indexed by
#' hyperedges. Each entry is non-zero if the associated vertex is a member of
#' the associated hyperedge. For an oriented hypergraph, this returns a list of
#' two matrices with the first representing incidence to one end of the
#' hyperedges and the second representing incidence to the other end. For a
#' directed hypergraph the first represents incidence to the tail of a
#' hyperedge and the second represents incidence to the head.
#'
#' It is hard to use the incidence matrices of oriented undirected hypergraphs
#' in calculations. The `augment_oriented` option turns the hypergraph into a
#' directed hypergraph, but each hyperedge is represented twice, once pointing
#' in each direction. This is much easier to use for further calculations.
#'
#' @param hype A hypergraph object
#' @param augment_oriented Whether to augment an oriented hypergraph
#' @param as_matrix Whether to coerce the result to a simple matrix
#'
#' @return An incidence matrix or a list of two incidence matrices.
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' incidence_matrix(h1)
#'
#' h2 <- example_hype(oriented = TRUE, directed = TRUE)
#' incidence_matrix(h2)
incidence_matrix <- function(hype, augment_oriented = TRUE, as_matrix = FALSE) {
  # If the hypergraph has real coefficients then return the saved incidence matrix
  if (hype$get_real_coef()) {
    if (as_matrix) {
      return(as.matrix(hype$get_inc_mat()))
    } else {
      return(hype$get_inc_mat())
    }
  }

  # Finding whether the hypergraph is oriented
  if (hype$get_oriented()) {
    # Getting basic hypergraph properties
    elist <- hype$get_elist()
    numv <- hype$get_numv()
    nume <- length(elist)

    # Initialising empty incidence matrix
    inc_mat <- list(
      Matrix::Matrix(0, nrow = numv, ncol = nume),
      Matrix::Matrix(0, nrow = numv, ncol = nume)
    )

    # Naming the matrices for directed hypergraphs
    if (hype$get_directed()) {
      names(inc_mat) <- c("from", "to")
    }

    # Setting the row and column names of the matrices
    rownames(inc_mat[[1]]) <- hype$get_vnames()
    colnames(inc_mat[[1]]) <- hype$get_enames()
    rownames(inc_mat[[2]]) <- hype$get_vnames()
    colnames(inc_mat[[2]]) <- hype$get_enames()

    # Iterating through the matrix columns and setting the relevant entries to 1
    for (i in 1:nume) {
      inc_mat[[1]][elist[[i]][[1]], i] <- 1
      inc_mat[[2]][elist[[i]][[2]], i] <- 1
    }

    # Augmenting oriented but undirected hypergraphs if necessary
    if (hype$get_oriented() & !hype$get_directed() & augment_oriented) {
      temp <- inc_mat[[1]]
      inc_mat[[1]] <- cbind(inc_mat[[1]], inc_mat[[2]])
      inc_mat[[2]] <- cbind(inc_mat[[2]], temp)
    }

    # Returning the incidence matrix
    if (as_matrix) {
      inc_mat[[1]] <- as.matrix(inc_mat[[1]])
      inc_mat[[2]] <- as.matrix(inc_mat[[2]])
    }
    return(inc_mat)
  } else {
    # Getting basic hypergraph properties
    elist <- hype$get_elist()
    numv <- hype$get_numv()
    nume <- length(elist)

    # Generating an empty incidence matrix
    inc_mat <- Matrix::Matrix(0, nrow = numv, ncol = nume)
    # Setting row and column names of the incidence matrix
    rownames(inc_mat) <- hype$get_vnames()
    colnames(inc_mat) <- hype$get_enames()

    # Iterating through the incidene matrix columns setting relevant entries to 1
    for (i in 1:nume) {
      inc_mat[elist[[i]], i] <- 1
    }

    # Return the incidence matrix
    if (as_matrix) {
      inc_mat <- as.matrix(inc_mat)
    }
    return(inc_mat)
  }
}

#' Find the Adjacency Matrix of a Hypergraph
#'
#' An adjacency matrix is a square matrix with both rows and columns being
#' indexed by vertices. For each entry, the number is proportional to the
#' strength of the connection going from the vertex represented as the row and
#' the vertex represented by the column. For undirected hypergraphs, this matrix
#' is symmetric but this is usually not the case for directed.
#'
#' Great care should be taken when using a hypergraph with mixed positive and
#' negative real coefficients as there is a chance no adjacency will be registered
#' for two adjacenct vertices. rhype does not check for these cases and they must
#' be checked for by the user.
#'
#' @param hype A hypergraph object
#' @param normalise Whether the matrix should be normalised to either 1 or 0
#' @param self_adj Whether self adjacency should be represented
#' @param as_matrix Whether the output should be coerced into a simple matrix
#'
#' @return A matrix of adjacencies between vertices of a hypergraph.
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' adjacency_matrix(h1)
#'
#' h2 <- example_hype(oriented = TRUE, directed = TRUE)
#' adjacency_matrix(h2)
adjacency_matrix <- function(hype, normalise = TRUE, self_adj = TRUE, as_matrix = FALSE) {
  # Finding the incidence matrix
  inc_mat <- incidence_matrix(hype)

  # Finding hyperedge weights
  if (hype$get_weighted()) {
    eweights <- hype$get_eweights()
  } else {
    eweights <- rep(1, length(hype$get_elist()))
  }

  # Finding the adjacency matrix using incidence matrices
  if (hype$get_oriented()) {
    adj_mat <- inc_mat[[1]] %*% (eweights * Matrix::t(inc_mat[[2]]))
  } else {
    adj_mat <- inc_mat %*% (eweights * Matrix::t(inc_mat))
  }

  # Setting the matrix entries to 0 and 1 if specified
  if (normalise) {
    adj_mat <- matrix(as.numeric(adj_mat != 0), nrow = dim(adj_mat)[1])
  }

  # Removing the leading diagonal if specified
  if (!self_adj) {
    Matrix::diag(adj_mat) <- 0
  }

  # Setting row and column names
  rownames(adj_mat) <- hype$get_vnames()
  colnames(adj_mat) <- hype$get_vnames()

  # Returning the adjacency matrix
  if (as_matrix) {
    adj_mat <- as.matrix(adj_mat)
  } else {
    # TODO work around for transposing sparse matrix problem. Fix problem and remove.
    adj_mat <- Matrix::Matrix(adj_mat)
  }

  return(adj_mat)
}

#' Find the Laplacian Matrix of a Hypergraph
#'
#' @param hype A hypergraph object
#'
#' @return The laplacian matrix of the hypergraph
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' laplacian_matrix(h1)
laplacian_matrix <- function(hype) {
  # Checking the hypergraph is not oriented
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is not yet available for oriented hypergraphs")
  }

  # Finding the adjacency matrix
  adj_mat <- adjacency_matrix(hype, normalise = FALSE, self_adj = FALSE)
  # Finding the row sum degree
  deg <- apply(adj_mat, 1, sum)
  # Transforming the adjacency matrix into the laplacian matrix
  lap_mat <- -1 * adj_mat
  Matrix::diag(lap_mat) <- deg
  # Returning the laplacian matrix
  return(lap_mat)
}

#' Find the Vertex Normalised Laplacian Matrix of a Hypergraph
#'
#' As defined by Jurgen Jost and Raffaella Mulas
#' \doi{10.1016/j.aim.2019.05.025}
#'
#' @param hype A hypergraph object
#'
#' @return The vertex normalised laplacian matrix of the hypergraph
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' vert_norm_lap_mat(h1)
vert_norm_lap_mat <- function(hype) {
  # Checking the hypergraph is not oriented
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is not yet available for oriented hypergraphs")
  }

  # Finding the laplacian matrix via the adjacency matrix
  lap_mat <- adjacency_matrix(hype, normalise = FALSE, self_adj = TRUE)
  lap_mat <- pseudo_invert(Matrix::diag(lap_mat)) * lap_mat
  # Returning the laplacian matrix
  return(lap_mat)
}

#' Find the Hyperedge Normalised Laplacian Matrix of a Hypergraph
#'
#' As defined by Jurgen Jost and Raffaella Mulas
#' \doi{10.1016/j.aim.2019.05.025}
#'
#' @param hype A hypergraph object
#'
#' @return The hyperedge normalised laplacian matrix of the hypergraph
#' @export
#'
#' @examples
#' h1 <- example_hype()
#' hype_norm_lap_mat(h1)
hype_norm_lap_mat <- function(hype) {
  # Checking the hypergraph is not oriented
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is not yet available for oriented hypergraphs")
  }

  # Calculating the laplacian matrix
  inc_mat <- incidence_matrix(hype)
  lap_mat <- Matrix::t(inc_mat) %*% (pseudo_invert(degree(hype, method = "hyperedge")) * inc_mat)
  # Returning the laplacian matrix
  return(lap_mat)
}
