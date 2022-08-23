test_that("getters and setters work", {

  hype <- example_hype()

  test_elist <- hyperedge_list(hype)
  real_elist <- list(
    h1 = 1:3,
    h2 = 2:4
  )
  expect_equal(test_elist, real_elist)

  test_vnames <- vertex_names(hype)
  real_vnames <- paste0("v", 1:4)
  expect_equal(test_vnames, real_vnames)

  expect_null(vertex_weights(hype))

  test_hnames <- hyperedge_names(hype)
  real_hnames <- paste0("h", 1:2)
  expect_equal(test_hnames, real_hnames)

  expect_null(hyperedge_weights(hype))

  expect_false(is_weighted(hype))

  expect_false(is_oriented(hype))

  expect_false(is_directed(hype))

  expect_false(has_real_coef(hype))

  expect_equal(hype_order(hype), 4)

  expect_equal(hype_size(hype), 2)

})
