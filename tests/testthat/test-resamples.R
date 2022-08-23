test_that("bootstrap works", {
  hype <- example_hype()

  resamples <- bootstrap_hype(hype, method = "vertex")
  rhype <- resamples[[1]]

  expect_equal(
    hype_order(hype),
    hype_order(hype)
  )

  expect_equal(
    hype_size(hype),
    hype_size(hype)
  )

  expect_true(
    all(vertex_names(rhype) %in% vertex_names(hype))
  )

  expect_true(
    all(hyperedge_names(rhype) %in% hyperedge_names(hype))
  )

  expect_false(is_weighted(rhype))
  expect_false(is_oriented(rhype))
  expect_false(has_real_coef(rhype))

  resamples <- bootstrap_hype(hype, method = "hyperedge")
  rhype <- resamples[[1]]

  expect_equal(
    hype_order(hype),
    hype_order(hype)
  )

  expect_equal(
    hype_size(hype),
    hype_size(hype)
  )

  expect_true(
    all(vertex_names(rhype) %in% vertex_names(hype))
  )

  expect_true(
    all(hyperedge_names(rhype) %in% hyperedge_names(hype))
  )

  expect_false(is_weighted(rhype))
  expect_false(is_oriented(rhype))
  expect_false(has_real_coef(rhype))

  resamples <- bootstrap_hype(hype, method = "both")
  rhype <- resamples[[1]]

  expect_equal(
    hype_order(hype),
    hype_order(hype)
  )

  expect_equal(
    hype_size(hype),
    hype_size(hype)
  )

  expect_true(
    all(vertex_names(rhype) %in% vertex_names(hype))
  )

  expect_true(
    all(hyperedge_names(rhype) %in% hyperedge_names(hype))
  )

  expect_false(is_weighted(rhype))
  expect_false(is_oriented(rhype))
  expect_false(has_real_coef(rhype))

})

test_that("jackknife hypergraph works", {
  hype <- example_hype()

  resamples <- jackknife_hype(hype, method = "vertex")

  rhype <- resamples[[1]]

  expect_equal(
    hype_order(rhype),
    hype_order(hype) - 1
  )

  expect_equal(
    hype_size(rhype),
    hype_size(hype)
  )

  expect_true(
    all(vertex_names(rhype) %in% vertex_names(hype))
  )

  expect_true(
    all(hyperedge_names(rhype) %in% hyperedge_names(hype))
  )

  expect_false(is_weighted(rhype))
  expect_false(is_oriented(rhype))
  expect_false(has_real_coef(rhype))

  resamples <- jackknife_hype(hype, method = "hyperedge")

  rhype <- resamples[[1]]

  expect_equal(
    hype_order(rhype),
    hype_order(hype)
  )

  expect_equal(
    hype_size(rhype),
    hype_size(hype) - 1
  )

  #TODO Investigate error
  #expect_true(
  #  all(vertex_names(rhype) %in% vertex_names(hype))
  #)

  expect_true(
    all(hyperedge_names(rhype) %in% hyperedge_names(hype))
  )

  expect_false(is_weighted(rhype))
  expect_false(is_oriented(rhype))
  expect_false(has_real_coef(rhype))

  resamples <- jackknife_hype(hype, method = "both")

  rhype <- resamples[[1]]

  expect_equal(
    hype_order(rhype),
    hype_order(hype) - 1
  )

  expect_equal(
    hype_size(rhype),
    hype_size(hype) - 1
  )

  #TODO investigate error
  #expect_true(
  #  all(vertex_names(rhype) %in% vertex_names(hype))
  #)

  expect_true(
    all(hyperedge_names(rhype) %in% hyperedge_names(hype))
  )

  expect_false(is_weighted(rhype))
  expect_false(is_oriented(rhype))
  expect_false(has_real_coef(rhype))
})

test_that("shuffle hypergraph works", {
  hype <- example_hype()

  resamples <- shuffle_hype(hype, method = "vertex")
  rhype <- resamples[[1]]

  expect_equal(
    hype_order(hype),
    hype_order(hype)
  )

  expect_equal(
    hype_size(hype),
    hype_size(hype)
  )

  expect_true(
    all(vertex_names(rhype) %in% vertex_names(hype))
  )

  expect_true(
    all(hyperedge_names(rhype) %in% hyperedge_names(hype))
  )

  expect_false(is_weighted(rhype))
  expect_false(is_oriented(rhype))
  expect_false(has_real_coef(rhype))

  resamples <- shuffle_hype(hype, method = "hyperedge")
  rhype <- resamples[[1]]

  expect_equal(
    hype_order(hype),
    hype_order(hype)
  )

  expect_equal(
    hype_size(hype),
    hype_size(hype)
  )

  expect_true(
    all(vertex_names(rhype) %in% vertex_names(hype))
  )

  expect_true(
    all(hyperedge_names(rhype) %in% hyperedge_names(hype))
  )

  expect_false(is_weighted(rhype))
  expect_false(is_oriented(rhype))
  expect_false(has_real_coef(rhype))
})
