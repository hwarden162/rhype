#' Quickly Validate a Hypergraph
#'
#' When using the rhype functions, the integrity of a hypergraph object should
#' remain intact. However, as the properties of a hypergraph object are
#' dependent on one another, it is possible in the case of an error or direct
#' object manipulation by the user that a hypergraph object's integrity is
#' corrupted. This will cause other rhype functions to either throw errors or to
#' calculate incorrect answers. This function is not exhaustive but will perform
#' multiple sanity checks on hypergraph objects and is a good place to start
#' when debugging.
#'
#' @param hype A hypergraph object
#' @param return A logical variable stating whether any output should be
#'     returned from the function
#' @param verbose A logical variable indicating whether the function should
#'     output text to the screen
#'
#' @return Outputs text to screen of any problems found within the hypergraph
#'     object. If `return` is set to `TRUE` then a logical output will be
#'     returned. This logical output will be `TRUE` if it passed all of the
#'     tests, `FALSE` if it failed any test that proves the structure of the
#'     hypergraph is broken or `NULL` if it failed a test that most hypergraphs
#'     used practically should pass, but doesn't necessarily mean the
#'     hypergraph is broken, see text output for more details.
#' @export
#'
#' @examples
#' h <- example_hype()
#' validate_hypergraph(h)
validate_hypergraph <- function(hype, return = FALSE, verbose = TRUE) {
  numv <- hype$get_numv()
  elist <- hype$get_elist()
  nume <- length(elist) # Equal to the number of hyperedges
  vnames <- hype$get_vnames()
  vweights <- hype$get_vweights()
  enames <- hype$get_enames()
  eweights <- hype$get_eweights()
  weighted <- hype$get_weighted()
  oriented <- hype$get_oriented()
  directed <- hype$get_directed()
  real_coef <- hype$get_real_coef()
  inc_mat <- hype$get_inc_mat()

  isValid <- TRUE
  major_faults <- 0
  minor_faults <- 0
  errorMessageMaj <- ""
  errorMessageMin <- ""

  # Checking numv exists
  if (is.null(numv)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Number of vertices is missing\n")
  }

  # Checking elist exists
  if (is.null(elist)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Hyperedge list is missing\n")
  }

  # Checking numv is an number
  if (!is.numeric(numv)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Number of vertices should be an integer\n")
  }

  # Checking elist is a list
  if (!is.list(elist)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Hyperedge list should be a list\n")
  }

  # Checking vnames is a vector of characters
  if (!is.character(vnames)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Vertex names should be a vector of characters\n")
  }

  # Checking vweights is a vector of numbers
  if (!is.numeric(vweights) & weighted) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Vertex weights should be a vector of numbers\n")
  }

  # Checking enames is a vector of characters
  if (!is.character(enames)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Hyperedge names should be a vector of characters\n")
  }

  # Checking eweights is a vector of numbers
  if (!is.numeric(eweights) & weighted) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Hyperedge weights should be a vector of numbers\n")
  }

  # Checking weighted is a logical value
  if (!is.logical(weighted)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Weighted should be a logical value\n")
  }

  # Checking oriented is a logical value
  if (!is.logical(oriented)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Oriented should be a logical value\n")
  }

  # Checking directed is a logical value
  if (!is.logical(directed)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Directed should be a logical value\n")
  }

  # Checking real_coef is a logical value
  if (!is.logical(real_coef)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Real coefficients should be a logical value\n")
  }

  # Checking inc_mat is a list, matrix or is null
  if (!(is.list(inc_mat) | is.matrix(inc_mat) | is.null(inc_mat))) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 Incidence matrix should either be a matrix, a list or NULL\n")
  }

  # Checking numv against elist
  if (!is.null(numv)) {
    if (numv != length(unique(unlist(elist)))) {
      if (isValid) {
        isValid <- NULL
      }
      minor_faults <- minor_faults + 1
      errorMessageMin <- paste(errorMessageMin, "\u2139 The number of vertices recorded and the number of vertices contained in the hyperedge list is different. This is expected if and only if you have an isolated vertex in your hypergraph. \n")
    }
  }

  # Checking numv against vnames
  if (!is.null(numv)) {
    if (numv != length(vnames)) {
      isValid <- FALSE
      major_faults <- major_faults + 1
      errorMessageMaj <- paste(errorMessageMaj, "\u2716 The number of vertices is not equal to the length of the vector containing the vertex names. \n")
    }
  }

  # Checking numv against vweights if weighted
  if (!is.null(numv)) {
    if ((numv != length(vweights)) & weighted) {
      isValid <- FALSE
      major_faults <- major_faults + 1
      errorMessageMaj <- paste(errorMessageMaj, "\u2716 The number of vertices is not equal to the length of the vector containing the vertex weights and the hypergraph is weighted. \n")
    }
  }

  # Checing numv against inc_mat
  if (real_coef) {
    if (oriented) {
      if ((dim(inc_mat[[1]])[1] != numv) | (dim(inc_mat[[2]])[1] != numv)) {
        isValid <- FALSE
        major_faults <- major_faults + 1
        errorMessageMaj <- paste(errorMessageMaj, "\u2716 The incidence matrix stored for real coefficients does not have the same number of rows as there are vertices in the hypergraph. \n")
      }
    } else {
      if (dim(inc_mat)[1]) {
        isValid <- FALSE
        major_faults <- major_faults + 1
        errorMessageMaj <- paste(errorMessageMaj, "\u2716 The incidence matrix stored for real coefficients does not have the same number of rows as there are vertices in the hypergraph. \n")
      }
    }
  }

  # Checking nume against elist
  if (nume != length(enames)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 The number of hyperedges is not equal to the length of vectors storing hyperedge names. \n")
  }

  # Checking elist names are stored
  if (is.null(names(elist))) {
    if (isValid) {
      isValid <- NULL
    }
    minor_faults <- minor_faults + 1
    errorMessageMin <- paste(errorMessageMin, "\u2139 The hyperedge names are not stored within the hyperedge list. \n")
  }

  # Checking nume against eweights
  if ((nume != length(eweights)) & weighted) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2139 The number of hyperedges is not equal to the number of the hyperedge weights stored. \n")
  }

  # Checking nume against inc_mat if the hypergraph has real coefficients
  if (real_coef) {
    if (oriented) {
      if ((dim(inc_mat[[1]])[2] != nume) | (dim(inc_mat[[2]])[2] != nume)) {
        isValid <- FALSE
        major_faults <- major_faults + 1
        errorMessageMaj <- paste(errorMessageMaj, "\u2716 The number of hyperedges is not equal to the number of columns in the stored incidence matrix. \n")
      }
    } else {
      if (dim(inc_mat)[2] != nume) {
        isValid <- FALSE
        major_faults <- major_faults + 1
        errorMessageMaj <- paste(errorMessageMaj, "\u2716 The number of hyperedges is not equal to the number of columns in the stored incidence matrix. \n")
      }
    }
  }

  # Checking vertex names are characters
  if (!is.character(vnames)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 The vertex names are not stored as characters. \n")
  }

  # Checking vertex names are unique
  if (length(vnames) != length(unique(vnames))) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 There are two or more vertices with the same name.\n")
  }

  # Checking hyperedge names are characters
  if (!is.character(enames) & !is.null(enames)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 The hyperedge names are not stored as characters. \n")
  }

  # Checking hyperedge names are unique
  if (length(enames) != length(unique(enames))) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 There are two or more hyperedges with the same name.\n")
  }

  # Checking weighted against vweights and eweights
  if (weighted) {
    if (is.null(vweights)) {
      isValid <- FALSE
      major_faults <- major_faults + 1
      errorMessageMaj <- paste(errorMessageMaj, "\u2716 The hyperedge is weighted but the vertex weights are missing. \n")
    }
    if (is.null(eweights)) {
      isValid <- FALSE
      major_faults <- major_faults + 1
      errorMessageMaj <- paste(errorMessageMaj, "\u2716 The hyperedge is weighted but the hyperedge weights are missing. \n")
    }
  } else {
    if (!is.null(vweights)) {
      isValid <- FALSE
      major_faults <- major_faults + 1
      errorMessageMaj <- paste(errorMessageMaj, "\u2716 The hyperedge is not weighted but the vertex weights are not NULL \n")
    }
    if (!is.null(eweights)) {
      isValid <- FALSE
      major_faults <- major_faults + 1
      errorMessageMaj <- paste(errorMessageMaj, "\u2716 The hyperedge is not weighted but the hyperedge weights are not NULL \n")
    }
  }

  # Checking oriented against elist
  if (length(elist) > 0) {
    if (oriented) {
      if (!all(as.logical(lapply(elist, is.list)), na.rm = TRUE)) {
        isValid <- FALSE
        major_faults <- major_faults + 1
        errorMessageMaj <- paste(errorMessageMaj, "\u2716 The hypergraph is oriented but not all the hyperedges are in oriented format.\n")
      }
    }
  }

  # Checking oriented against directed
  if (!oriented & directed) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 The hypergraph is directed but not oriented.\n")
  }

  # Checking oriented against inc_mat
  if (real_coef & oriented) {
    if (!is.list(inc_mat)) {
      isValid <- FALSE
      major_faults <- major_faults + 1
      errorMessageMaj <- paste(errorMessageMaj, "\u2716 The hypergraph is oriented but the stored incidence matrix is not in oriented format.")
    }
  }

  # Checking if a directed hypergraph has hyperedge end names
  if (directed) {
    if (!all(as.logical(lapply(elist, function(x) {
      names(x) == c("from", "to")
    })), na.rm = TRUE)) {
      if (isValid) {
        isValid <- NULL
      }
      minor_faults <- minor_faults + 1
      errorMessageMin <- paste(errorMessageMin, "\u2139 The hypergraph is directed but the names in the hyperedges are missing \"from\" and \"to\".\n")
    }
  }

  # Checking directed against inc_mat
  if (directed & real_coef) {
    if (is.null(names(inc_mat))) {
      if (isValid) {
        isValid <- NULL
      }
      minor_faults <- minor_faults + 1
      errorMessageMin <- paste(errorMessageMin, "\2139 The hypergraph is directed but the saved incidence matrix does not have \"from\" and \"to\" as names.\n")
    } else {
      if (all(names(inc_mat) != c("from", "to"))) {
        if (isValid) {
          isValid <- NULL
        }
        minor_faults <- minor_faults + 1
        errorMessageMin <- paste(errorMessageMin, "\2139 The hypergraph is directed but the saved incidence matrix does not have \"from\" and \"to\" as names.\n")
      }
    }
  }

  # Checking real_coef against inc_mat
  if (real_coef & is.null(inc_mat)) {
    isValid <- FALSE
    major_faults <- major_faults + 1
    errorMessageMaj <- paste(errorMessageMaj, "\u2716 The hypergraph has real coefficients but there is no incidence matrix stored to save them.\n")
  }

  # Formatting output message
  errorMessageMaj <- paste("  There are", major_faults, "serious problems with this hypergraph:\n", errorMessageMaj)
  errorMessageMin <- paste("There are", minor_faults, "items that need your attention with this hypergraph:\n", errorMessageMin)
  errorMessage <- paste(errorMessageMaj, errorMessageMin, "These tests are not exhaustive, just an indication of where things might be going wrong.")

  if (verbose) {
    cat(errorMessage)
  }

  if (return) {
    return(isValid)
  }
}
