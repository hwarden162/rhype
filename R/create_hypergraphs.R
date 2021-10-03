hype_from_edge_list <- function(elist, directed = FALSE) {

  if (length(elist) == 0) {
    warning("\n Hyperedge list is empty, it is assumed that the hypergraph is unoriented")
    Hypergraph$new(
      numv = 0,
      elist = list()
    )
  }

  verts <- unique(unlist(elist))

  numv <- length(verts)
  vnames <- as.character(verts)

  if (!is.null(names(elist))) {
    enames <- names(elist)
  } else {
    enames <- NULL
  }

  if (is.list(elist[[1]])) {

# Pick up from here -------------------------------------------------------


  }
}
