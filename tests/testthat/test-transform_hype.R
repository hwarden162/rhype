test_that("creating dual hypergraph works", {
  hype <- example_hype()

  test_dual <- dual_hype(hype)

  expect_equal(test_dual$get_numv(), 2)

  expect_equal(
    test_dual$get_elist(),
    list(
      v1 = "h1",
      v2 = c("h1", "h2"),
      v3 = c("h1", "h2"),
      v4 = "h2"
    )
  )

  expect_equal(
    test_dual$get_vnames(),
    c("h1", "h2")
  )

  expect_null(test_dual$get_vweights())

  expect_equal(
    test_dual$get_enames(),
    paste0("v", 1:4)
  )

  expect_null(test_dual$get_eweights())

  expect_false(test_dual$get_weighted())
  expect_false(test_dual$get_oriented())
  expect_false(test_dual$get_directed())
  expect_false(test_dual$get_real_coef())
  expect_null(test_dual$get_inc_mat())
})

test_that("partial hypergraph code works", {
  hype <- example_hype()
  part_hype <- partial_hype(hype, "h1")

  expect_equal(part_hype$get_numv(), 4)

  expect_equal(
    part_hype$get_elist(),
    list(
      h1 = paste0("v", 1:3)
    )
  )
})
