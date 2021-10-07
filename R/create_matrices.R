#' Pseudo-Invert a Vector
#'
#' @param vec A vector of numbers
#'
#' @return A vector of pseudo-inverted numbers
pseudo_invert <- function(vec) {
  for (i in 1:length(vec)) {
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
#'
#' @return An incidence matrix or a list of two incidence matrices.
#' @export
#'
#' @examples
incidence_matrix <- function(hype, augment_oriented = TRUE) {
  if (hype$get_real_coef()) {
    return(hype$get_inc_mat())
  }

  if (hype$get_oriented()) {
    elist <- hype$get_elist()
    numv <- hype$get_numv()
    nume <- length(elist)

    inc_mat <- list(
      matrix(0, nrow = numv, ncol = nume),
      matrix(0, nrow = numv, ncol = nume)
    )

    if (hype$get_directed()) {
      names(inc_mat) <- c("from", "to")
    }

    rownames(inc_mat[[1]]) <- hype$get_vnames()
    colnames(inc_mat[[1]]) <- hype$get_enames()
    rownames(inc_mat[[2]]) <- hype$get_vnames()
    colnames(inc_mat[[2]]) <- hype$get_enames()

    for (i in 1:nume) {
      inc_mat[[1]][elist[[i]][[1]], i] <- 1
      inc_mat[[2]][elist[[i]][[2]], i] <- 1
    }

    if (hype$get_oriented() & !hype$get_directed() & augment_oriented) {
      temp <- inc_mat[[1]]
      inc_mat[[1]] <- cbind(inc_mat[[1]], inc_mat[[2]])
      inc_mat[[2]] <- cbind(inc_mat[[2]], temp)
    }

    return(inc_mat)
  } else {
    elist <- hype$get_elist()
    numv <- hype$get_numv()
    nume <- length(elist)

    inc_mat <- matrix(0, nrow = numv, ncol = nume)
    rownames(inc_mat) <- hype$get_vnames()
    colnames(inc_mat) <- hype$get_enames()

    for (i in 1:nume) {
      inc_mat[elist[[i]], i] <- 1
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
#' for two adjacenct vertices. hypR does not check for these cases and they must
#' be checked for by the user.
#'
#' @param hype A hypergraph object
#' @param normalise Whether the matrix should be normalised to either 1 or 0
#' @param self_adj Whether self adjacency should be represented
#'
#' @return A matrix of adjacencies between vertices of a hypergraph.
#' @export
#'
#' @examples
adjacency_matrix <- function(hype, normalise = TRUE, self_adj = TRUE) {
  inc_mat <- incidence_matrix(hype)

  if (hype$get_weighted()) {
    eweights <- hype$get_eweights()
  } else {
    eweights <- rep(1, length(hype$get_elist()))
  }

  if (hype$get_oriented()) {
    adj_mat <- inc_mat[[1]] %*% (eweights * t(inc_mat[[2]]))
  } else {
    adj_mat <- inc_mat %*% (eweights * t(inc_mat))
  }

  if (normalise) {
    adj_mat <- matrix(as.numeric(adj_mat != 0), nrow = dim(adj_mat)[1])
  }

  if (!self_adj) {
    diag(adj_mat) <- 0
  }

  rownames(adj_mat) <- hype$get_vnames()
  colnames(adj_mat) <- hype$get_vnames()

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
laplacian_matrix <- function(hype) {
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is not yet available for oriented hypergraphs")
  }

  adj_mat <- adjacency_matrix(hype, normalise = FALSE, self_adj = FALSE)
  deg <- apply(adj_mat, 1, sum)
  lap_mat <- -1 * adj_mat
  diag(lap_mat) <- deg
  return(lap_mat)
}

#' Find the Vertex Normalised Laplacian Matrix of a Hypergraph
#'
#' @param hype A hypergraph object
#'
#' @return The vertex normalised laplacian matrix of the hypergraph
#' @export
#'
#' @examples
vert_norm_lap_mat <- function(hype) {
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is not yet available for oriented hypergraphs")
  }

  lap_mat <- adjacency_matrix(hype, normalise = FALSE, self_adj = TRUE)
  lap_mat <- pseudo_invert(diag(lap_mat)) * lap_mat
  return(lap_mat)
}

#' Find the Hyperedge Normalised Laplacian Matrix of a Hypergraph
#'
#' @param hype A hypergraph object
#'
#' @return The hyperedge normalised laplacian matrix of the hypergraph
#' @export
#'
#' @examples
hype_norm_lap_mat <- function(hype) {
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is not yet available for oriented hypergraphs")
  }

  inc_mat <- incidence_matrix(hype)
  lap_mat <- t(inc_mat) %*% (pseudo_invert(degree(hype, method = "hyperedge")) * inc_mat)
  return(lap_mat)
}
