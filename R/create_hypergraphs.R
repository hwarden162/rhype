#' Create a Hypergraph From a Hyperedge List
#'
#' @param elist A hyperedge list. For an unoriented hypergraph, a hyperedge is
#' just a vector of the vertices contained within the hyperedge. Each vertex is
#' represented as a string. For an oriented hypergraph, each hyperedge is
#' itself a list of two vectors. Each of these vectors contains strings
#' representing the vertices contained in one end of the hyperedge. For a
#' directed hypergraph, each hyperedge is also a list of two vectors. In the
#' directed case, the first vector represents the vertices contained in the
#' tail of the hyperedge and the second the vertices contained in the head.
#' These two entries are also named `from` and `to`.
#' @param directed A logical value representing whether the hypergraph should
#' be directed.
#'
#' @return A hypergraph object with the given hyperedge structure.
#' @export
#'
#' @examples
hype_from_edge_list <- function(elist, directed = FALSE) {

  # Checking whether the hypergraph has any hyperedges
  if (length(elist) == 0) {
    warning("\n Hyperedge list is empty, it is assumed that the hypergraph is unoriented")
    # Returning the empty hypergaph
    return(
      Hypergraph$new(
        numv = 0,
        elist = list()
      )
    )
  }

  # Finding all of the individual vertices
  verts <- unique(unlist(elist))

  # Finding basic hypergraph properties from the vertices
  numv <- length(verts)
  vnames <- as.character(verts)

  # Checking whether hyperedge names have been supplied
  if (!is.null(names(elist))) {
    enames <- names(elist)
  } else {
    enames <- NULL
  }

  # Checking whether the first entry is a list
  if (is.list(elist[[1]])) {
    oriented <- TRUE
  } else {
    oriented <- FALSE
  }

  # Checking to see an unoriented hypergraph hasn't been specifies as directed
  if (directed & !oriented) {
    warning("\n Hyperedges not supplied in an orientable format, unoriented/undirected hypergraph being created instead")
    directed <- FALSE
  }

  # Converting all vertex names to strings
  if (oriented) {
    elist <- lapply(
      elist,
      function(x) {
        entry1 <- as.character(x[[1]])
        entry2 <- as.character(x[[2]])
        list(entry1, entry2)
      }
    )
  } else {
    elist <- lapply(elist, as.character)
  }

  # Renaming directed hyperedges to indicate which is head and which is tail
  if (directed) {
    elist <- lapply(
      elist,
      function(x) {
        names(x) <- c("from", "to")
        x
      }
    )
  }

  # Returning the hypergraph
  Hypergraph$new(
    numv = numv,
    elist = elist,
    vnames = vnames,
    enames = enames,
    oriented = oriented,
    directed = directed
  )
}

#' Create a Hypergraph From an Incidence Matrix
#'
#' @param inc_mat An incidence matrix or, for an oriented hypergraph, a list
#'     of two incidence matrices.
#' @param directed A logical value representing whether the hypergraph should
#'     be directed.
#' @param real_coef A logical value representing whether the hypergraph should
#'     have real coefficients associating vertices to hyperedges.
#'
#' @return A hypergraph object with the given incidence structure.
#' @export
#'
#' @examples
hype_from_inc_mat <- function(inc_mat, directed = FALSE, real_coef = FALSE) {
  # Checking if the incidence matrix is for an oriented hypergraph
  if (is.list(inc_mat)) {
    # Checking object is a matrix
    inc_mat[[1]] <- as.matrix(inc_mat[[1]])
    inc_mat[[2]] <- as.matrix(inc_mat[[2]])

    # Finding the number of vertices
    numv <- dim(inc_mat[[1]])[1]

    # Finding the names of the vertices, or assigning them if they don't exist
    if (!is.null(rownames(inc_mat[[1]]))) {
      vnames <- as.character(rownames(inc_mat[[1]]))
    } else {
      vnames <- as.character(1:numv)
    }

    # Finding tail elements of hyperedges
    tail_list <- list()
    for (i in 1:dim(inc_mat[[1]])[2]) {
      tail_list[[i]] <- vnames[which(inc_mat[[1]][,i] != 0)]
    }
    # Finding head elements of hyperedges
    head_list <- list()
    for (i in 1:dim(inc_mat[[2]])[2]) {
      head_list[[i]] <- vnames[which(inc_mat[[2]][,i] != 0)]
    }

    # Generating hyperedge list
    elist <- list()
    for (i in 1:length(tail_list)) {
      elist[[i]] <- list(tail_list[[i]], head_list[[i]])
      if (directed) {
        names(elist[[i]]) <- c("from", "to")
      }
    }

    # Finding hyperedge names
    if (!is.null(colnames(inc_mat[[1]]))) {
      enames <- as.character(colnames(inc_mat[[1]]))
      names(elist) <- enames
    } else {
      enames <- NULL
    }

    # Setting the hypergraph as oriented
    oriented <- TRUE

    # Removing the incidence matrix if real_coef is false
    if (!real_coef) {
      inc_mat <- NULL
    }

    # Returning a hypergraph object
    return(
      Hypergraph$new(
        numv = numv,
        elist = elist,
        vnames = vnames,
        enames = enames,
        oriented = oriented,
        directed = directed,
        real_coef = real_coef,
        inc_mat = inc_mat
      )
    )
  } else {
    # Converting parameter to a matrix
    inc_mat <- as.matrix(inc_mat)

    # Finding the number of vertices
    numv <- dim(inc_mat)[1]

    # Finding vertex names
    if (!is.null(rownames(inc_mat))) {
      vnames <- as.character(rownames(inc_mat))
    } else {
      vnames <- as.character(1:numv)
    }

    # Generating hyperedge list
    elist <- apply(inc_mat, 2, function(x) {
      which(x != 0)
    })

    # Finding hyperedge names
    if (!is.null(colnames(inc_mat))) {
      enames <- as.character(colnames(inc_mat))
      names(elist) <- enames
    } else {
      enames <- NULL
    }

    # Setting the hypergraph as unoriented
    oriented <- FALSE

    # If directed is set true but hypergraph is unoriented, outputting warning
    if (directed) {
      warning("\n Specified incidnece matrix is non-orientable, so can't be directed. Creating unoriented hypergraph instead.")
      directed <- FALSE
    }

    # If real_coef is false, setting the incidence matrix to NULL
    if (!real_coef) {
      inc_mat <- NULL
    }

    # Returning the hypergraph
    return(
      Hypergraph$new(
        numv = numv,
        elist = elist,
        vnames = vnames,
        enames = enames,
        oriented = oriented,
        directed = directed,
        real_coef = real_coef,
        inc_mat = inc_mat
      )
    )
  }
}
