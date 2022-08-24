test_that("create hypergraph from edge list works", {
  elist <- list(
    h1 = paste0("v", 1:3),
    h2 = paste0("v", 2:4)
  )

  test_hype <- hype_from_edge_list(elist)

  expect_equal(
    test_hype$get_numv(),
    4
  )

  expect_equal(
    test_hype$get_elist(),
    list(
      h1 = paste0("v", 1:3),
      h2 = paste0("v", 2:4)
    )
  )

  expect_equal(
    test_hype$get_vnames(),
    paste0("v", 1:4)
  )

  expect_null(test_hype$get_vweights())

  expect_equal(
    test_hype$get_enames(),
    paste0("h", 1:2)
  )

  expect_null(test_hype$get_eweights())

  expect_false(test_hype$get_weighted())
  expect_false(test_hype$get_oriented())
  expect_false(test_hype$get_directed())
  expect_false(test_hype$get_real_coef())
  expect_null(test_hype$get_inc_mat())

})

test_that("create hypergraph from incidence matrix works", {

  inc_mat <- matrix(
    c(1,1,1,0,0,1,1,1),
    nrow = 4, ncol = 2,
    dimnames = list(
      paste0("v", 1:4),
      paste0("h", 1:2)
    )
  )

  test_hype <- hype_from_inc_mat(inc_mat)

  expect_equal(
    test_hype$get_numv(),
    4
  )

  expect_equal(
    test_hype$get_elist(),
    list(
      h1 = paste0("v", 1:3),
      h2 = paste0("v", 2:4)
    )
  )

  expect_equal(
    test_hype$get_vnames(),
    paste0("v", 1:4)
  )

  expect_null(test_hype$get_vweights())

  expect_equal(
    test_hype$get_enames(),
    paste0("h", 1:2)
  )

  expect_null(test_hype$get_eweights())

  expect_false(test_hype$get_weighted())
  expect_false(test_hype$get_oriented())
  expect_false(test_hype$get_directed())
  expect_false(test_hype$get_real_coef())
  expect_null(test_hype$get_inc_mat())

})
