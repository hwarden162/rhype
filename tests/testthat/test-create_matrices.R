test_that("pseudo_invert works", {
  vec <- c(0,1,2,4,8)
  inv_vec <- pseudo_invert(vec)
  expect_equal(inv_vec, c(0,1,0.5,0.25,0.125))
})

test_that("incidence_matrix works for undirected hypergraphs", {
  h1 <- example_hype()
  real_inc_mat <- incidence_matrix(h1)
  check_inc_mat <- matrix(
    c(1,1,1,0,0,1,1,1),
    ncol = 2,
    nrow = 4,
    dimnames = list(
      c("v1","v2","v3","v4"),
      c("h1","h2")
    )
  )
  expect_equal(real_inc_mat, check_inc_mat)
})

test_that("adjacency_matrix works for undirected hypergraph", {
  h1 <- example_hype()
  real_adj_mat <- adjacency_matrix(h1)
  check_adj_mat <- matrix(
    c(0,1,1,0,1,0,2,1,1,2,0,1,0,1,1,0),
    ncol = 4, nrow = 4,
    dimnames = list(
      c("v1","v2","v3","v4"),
      c("v1","v2","v3","v4")
    )
  )
  expect_equal(real_adj_mat, check_adj_mat)
})
