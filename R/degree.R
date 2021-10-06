degree <- function(hype, method = "vertex") {
  if (hype$get_oriented()) {
    stop("\n \u2716 Degree not yet supported for oriented hypergraphs")
  }

  if (method == "vertex") {
    adj_mat <- adjacency_matrix(hype, normalise = FALSE, self_adj = FALSE)
    return(
      apply(adj_mat, 1, sum)
    )
  } else if (method == "vertex_simple") {
    adj_mat <- adjacency_matrix(hype, normalise = TRUE, self_adj = FALSE)
    return(apply(adj_mat, 1, sum))
  } else if (method == "hyperedge") {
    adj_mat <- adjacency_matrix(hype, normalise = FALSE, self_adj = TRUE)
    return(diag(adj_mat))
  }
}
