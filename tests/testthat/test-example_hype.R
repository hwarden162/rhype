test_that("hypergraph example works", {

  numv = 4

  elist <- list(
    paste0("v", 1:3),
    paste0("v", 2:4)
  )

  vnames <- paste0("v", 1:4)
  enames <- paste0("h", 1:2)
  names(elist) <- enames

  vweights <- NULL
  eweights <- NULL

  real_coef <- FALSE
  inc_mat <- NULL

  expect_equal(
    example_hype(),
    Hypergraph$new(
      numv = numv,
      elist = elist,
      vnames = vnames,
      vweights = vweights,
      enames = enames,
      eweights = eweights,
      weighted = FALSE,
      oriented = FALSE,
      directed = FALSE,
      real_coef = real_coef,
      inc_mat = inc_mat
    )
  )

})
