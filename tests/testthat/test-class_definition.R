test_that("class definition works", {

  numv <- 4

  elist <- list(
    paste0("v", 1:3),
    paste0("v", 2:4)
  )

  vnames <- paste0("v", 1:4)
  enames <- paste0("h", 1:2)
  names(elist) <- enames

  vweights <- NULL
  eweights <- NULL

  weighted <- FALSE
  oriented <- FALSE
  directed <- FALSE
  real_coef <- FALSE
  inc_mat <- NULL

  hype <- Hypergraph$new(
    numv = numv,
    elist = elist,
    vnames = vnames,
    vweights = vweights,
    enames = enames,
    eweights = eweights,
    weighted = weighted,
    oriented = oriented,
    directed = directed,
    real_coef = real_coef,
    inc_mat = inc_mat
  )

  expect_equal(
    hype$get_numv(),
    4
  )

  expect_equal(
    hype$get_elist(),
    list(
      h1 = c("v1", "v2", "v3"),
      h2 = c("v2", "v3", "v4")
    )
  )

  expect_equal(
    hype$get_vnames(),
    paste0("v", 1:4)
  )

  expect_null(hype$get_vweights())

  expect_equal(
    hype$get_enames(),
    paste0("h", 1:2)
  )

  expect_null(hype$get_eweights())

  expect_false(hype$get_weighted())
  expect_false(hype$get_oriented())
  expect_false(hype$get_directed())
  expect_false(hype$get_real_coef())
  expect_null(hype$get_inc_mat())

})
