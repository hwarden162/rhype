incidence_matrix <- function(hype, augment_oriented = TRUE) {
  if (hype$get_real_coef()) {
    return(hype$get_inc_mat())
  }

  if (hype$get_oriented()) {

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
