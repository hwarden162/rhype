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
