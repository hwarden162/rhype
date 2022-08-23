#' Bootstrap A Hypergraph
#'
#' Bootstrapping is a common statistical technique used to quantify
#' uncertainty of calculations. This is an approximation of the
#' bootstrap algorithm for hypergraphs. Bootstrapping is achieved
#' by creating a new hypergraph where the vertices, hyperedges or
#' both have themselves been bootstrapped, achieved using the
#' `"vertex"`, `"hyperedge"` or `"both"` methods.
#'
#' @param hype A hypergraph object.
#' @param n The number of bootstrapped hypergraphs required.
#' @param method What method to use to calculate the bootstrapped
#' hypergraphs
#'
#' @return A list of bootstrapped hypergraphs.
#' @export
#'
#' @examples
#' hype <- example_hype()
#' resamples <- bootstrap_hype(hype, n = 5)
#' lapply(resamples, incidence_matrix)
bootstrap_hype <- function(hype, n = 1, method = "both") {
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is currently not available for oriented hypergraphs")
  }
  if (hype$get_weighted()) {
    stop("\n \u2716 This function is currently not available for weighted hypergraphs")
  }
  numv <- hype$get_numv()
  nume <- length(hype$get_elist())
  inc_mat <- incidence_matrix(hype)
  hype_list <- list()
  for (i in 1:n) {
    if (method == "both" | method == "vertex") {
      v_inds <- sample(1:numv, numv, replace = TRUE)
    } else {
      v_inds <- 1:numv
    }
    if (method == "both" | method == "hyperedge") {
      e_inds <- sample(1:nume, nume, replace = TRUE)
    } else {
      e_inds <- 1:nume
    }
    new_inc_mat <- inc_mat[v_inds, e_inds]
    new_hype <- hype_from_inc_mat(new_inc_mat)
    hype_list[[i]] <- new_hype
  }
  return(hype_list)
}

#' Jackknife A Hypergraph
#'
#' Jackknifing is a resampling technique similar to bootstrapping, where many
#' resamples are taken, each time leaving out one observation. For the abstraction
#' to hypergraphs, the `"vertex"` method recreates the hypergraph leaving out one
#' vertex, the `"hyperedge"` method recreates the hypergraph leaving out one hyperedge
#' and the `"both"` method leaves out one of each.
#'
#' @param hype A hypergraph object.
#' @param n The number of hypergraphs to create.
#' @param method The method to use to jackknife the hypergraphs.
#'
#' @return A list of jackknifed hypergraphs.
#' @export
#'
#' @examples
#' hype <- example_hype()
#' resamples <- jackknife_hype(hype, n = 5)
#' lapply(resamples, incidence_matrix)
jackknife_hype <- function(hype, n = 1, method = "both") {
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is currently not available for oriented hypergraphs")
  }
  if (hype$get_weighted()) {
    stop("\n \u2716 This function is currently not available for weighted hypergraphs")
  }
  numv <- hype$get_numv()
  nume <- length(hype$get_elist())
  inc_mat <- incidence_matrix(hype)
  hype_list <- list()
  for (i in 1:n) {
    v_inds <- 1:numv
    e_inds <- 1:nume
    if (method == "both" | method == "vertex") {
      v_inds <- v_inds[-sample(numv, 1)]
    }
    if (method == "both" | method == "hyperedge") {
      e_inds <- e_inds[-sample(nume,1)]
    }
    new_inc_mat <- inc_mat[v_inds, e_inds]
    new_hype <- hype_from_inc_mat(new_inc_mat)
    hype_list[[i]] <- new_hype
  }
  return(hype_list)
}

#' Shuffle A Hypergraph
#'
#' A hypergraph can be shuffled to slightly perturb its structure. These shuffled
#' hypergraphs can then be used to estimate the uncertainty of calculations on
#' the original hypergraph.
#'
#' Two methods are used to shuffle a hypergraph, the `"vertex"` method keeps the
#' degree of each vertex the same, randomly reassigning the hyperedges they are
#' members of. The `"hyperedge"` method keeps the cardinality of each hyperedge
#' the same, randomly reassigning the vertices that are members.
#'
#' @param hype A hypergraph object.
#' @param n The number of shuffled hypergraphs to calculate.
#' @param method The method to use to shuffle the hypergraph.
#'
#' @return A list of shuffled hypergraphs.
#' @export
#'
#' @examples
#' hype <- example_hype()
#' resamples <- shuffle_hype(hype, n = 5)
#' lapply(resamples, incidence_matrix)
shuffle_hype <- function(hype, n = 1, method = "hyperedge") {
  if (hype$get_oriented()) {
    stop("\n \u2716 This function is currently not available for oriented hypergraphs")
  }
  if (hype$get_weighted()) {
    stop("\n \u2716 This function is currently not available for weighted hypergraphs")
  }
  numv <- hype$get_numv()
  nume <- length(hype$get_elist())
  inc_mat <- incidence_matrix(hype)
  hype_list <- list()
  for (i in 1:n) {
    new_inc_mat <- inc_mat
    if (method == "hyperedge") {
      for(j in 1:nume) {
        new_inc_mat[,j] <- new_inc_mat[sample(1:numv, numv),j]
      }
    }
    if (method == "vertex") {
      for(j in 1:numv) {
        new_inc_mat[j,] <- new_inc_mat[j,sample(1:nume, nume)]
      }
    }
    new_hype <- hype_from_inc_mat(new_inc_mat)
    hype_list[[i]] <- new_hype
  }
  return(hype_list)
}
