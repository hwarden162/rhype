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
    Hypergraph$new(
      numv = 0,
      elist = list()
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
        entry1 <- lapply(x[[1]], as.character)
        entry2 <- lapply(x[[2]], as.character)
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
