#' Find The Support Graph Of A Hypergraph
#'
#' The support graph of a hypergraph is a graph that has a vertex for every vertex
#' in the hypergraph. Two vertices are connected in the support graph if there is a
#' hyperedge that connects them.
#'
#' If `simple` is set to `FALSE` then for unweighted hypergraphs without real coefficients
#' the support graph has an edge connected vertices for each hyperedge connecting them
#' in the hypergraph.
#'
#' @param hype A hypergraph object.
#' @param simple Whether a simplified support graph should be created.
#'
#' @return The support graph of the hypergraph
#' @export
#'
#' @examples
#' hype <- example_hype()
#' g <- support_graph(hype)
#' print(g)
#' plot(g)
support_graph <- function(hype, simple = TRUE) {
  if (hype$get_oriented()) {
    stop("\n \u2716 This funciton is not currently supported for oriented hypegraphs")
  }
  if ((hype$get_weighted() | hype$get_real_coef()) & !simple) {
    stop("\n \u2716 This function only supports calculating simple support graphs for weighted hypergraphs or hypergraphs with real coefficients")
  }
  adj_mat <- adjacency_matrix(hype)
  if (simple) {
    adj_mat <- matrix(
      as.numeric(adj_mat != 0),
      nrow = nrow(adj_mat), ncol = ncol(adj_mat),
      dimnames = dimnames(adj_mat)
    )
  }
  supp_g <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected")
  return(supp_g)
}

#' Find The Connectivity Graph Of A Hypergraph
#'
#' The connectivity graph is a graphical representation of a hypergraph, it has a vertex
#' for each vertex and hyperedge in the hypergraph. Two hyperedges are connected in the
#' connectivity graph if they both have a vertex in common, a vertex is connected to  a
#' hyperedge if the vertex is contained in the hyperedge and no vertices are connected by
#' edges.
#'
#' @param hype A hypergraph object.
#'
#' @return A graph object representing the hyperedge connectivity graph.
#' @export
#'
#' @examples
#' hype <- example_hype()
#' g <- connectivity_graph(hype)
#' print(g)
#' plot(g)
connectivity_graph <- function(hype) {
  if (hype$get_oriented()) {
    stop("\n \u2716 This funciton is not currently supported for oriented hypegraphs")
  }
  supp_g <- support_graph(hype_dual(hype), simple = TRUE)
  igraph::V(supp_g)$color <- "blue"
  igraph::V(supp_g)$source <- "hyperedge"
  supp_g <- igraph::add_vertices(
    supp_g,
    nv = hype$get_numv(),
    attr = list(
      name = hype$get_vnames(),
      color = "red",
      source = "vertex"
    )
  )
  inc_mat <- incidence_matrix(hype)
  for (v_name in hype$get_vnames()) {
    e_inds <- which(inc_mat[v_name,] != 0)
    e_names <- hype$get_enames()[e_inds]
    v_names <- rep(v_name, length(e_names))
    e_list <- c(rbind(v_names, e_names))
    supp_g <- igraph::add_edges(supp_g, e_list)
  }
  return(supp_g)
}
