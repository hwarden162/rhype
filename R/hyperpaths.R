#' Find The Shortest Hyperpaths Between Two Vertices
#'
#' A hyperpath is a set of hyperedges such that each consecutive pair of hyperedges
#' contain at least vertex in common. A shortest hyperpath between two vertices is
#' the smallest set of hyperedges that form a hyperpath such that one vertex is in
#' the first hyperpath and the other vertex is in the last hyperpath.
#'
#' @param hype A hypergraph object.
#' @param from The vertex that is the start of the hyperpath.
#' @param to  The vertex that is the end of the hyperpath,
#'
#' @return A list of shortest hyperpaths between the given vertices.
#' @export
#'
#' @examples
#' hype <- example_hype()
#' shortest_hyperpaths(hype, "v1", "v4")
shortest_hyperpaths <- function(hype, from, to) {
  con_graph <- connectivity_graph(hype)
  paths <- igraph::all_shortest_paths(
    con_graph,
    from = from,
    to = to
  )
  paths <- lapply(
    paths$res,
    function(x) {
      l <- length(x)
      p <- x[-c(1,l)]
      p
    }
  )
  paths
}
