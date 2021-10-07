spectra <- function(hype, matrix = "laplacian") {
  if (matrix == "laplacian") {
    return(eigen(laplacian_matrix(hype)))
  } else if (matrix == "adjacency") {
    return(eigen(adjacency_matrix(hype, normalise = FALSE, self_adj = TRUE)))
  } else if (matrix == "vert_norm_lap_mat") {
    return(eigen(vert_norm_lap_mat(hype)))
  } else if (matrix == "hype_norm_lap_mat") {
    return(eigen(hype_norm_lap_mat(hype)))
  }
}

spectral_distance <- function(hype1, hype2, matrix = "laplacian") {
  eigen1 <- spectra(hype1, matrix)$values
  eigen2 <- spectra(hype2, matrix)$values

  spec_dist <- sum(abs(eigen1 - eigen2))
  return(spec_dist)
}

spectral_distance_disc <- function(hype, matrix = "vert_norm_lap_mat") {
  if ((matrix != "vert_norm_lap_mat") & (matrix != "hype_norm_lap_mat")) {
    stop("\n \u2716 Function only valid for vertex/hyperedge normalised laplacians")
  }

  eigen1 <- spectra(hype, matrix)$values
  eigen2 <- rep(1, length(eigen1))

  spec_dist <- sum(abs(eigen1 - eigen2))
  return(spec_dist)
}
