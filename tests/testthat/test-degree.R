# Testing Degree ----------------------------------------------------------

test_that("degree function works on unoriented hypergraphs", {
  hype <- example_hype()
  hype_weighted <- example_hype(edge_weighted = TRUE)
  expect_warning(degree(hype))

  # Test vertex degree
  test_deg <- degree(hype, "vertex")
  real_deg <- c(2,3,3,2)
  names(real_deg) <- c("v1","v2","v3","v4")
  expect_equal(test_deg, real_deg)

  test_deg_weighted <- degree(hype_weighted, "vertex")
  real_deg_weighted <- c(2,3,3,2)
  names(real_deg_weighted) <- c("v1","v2","v3","v4")
  expect_equal(test_deg_weighted, real_deg)

  # Test vertex simple degree
  test_deg <- degree(hype, "vertex_simple")
  real_deg <- c(2,3,3,2)
  names(real_deg) <- c("v1","v2","v3","v4")
  expect_equal(test_deg, real_deg)

  test_deg_weighted <- degree(hype_weighted, "vertex_simple")
  real_deg_weighted <- c(2,3,3,2)
  names(real_deg_weighted) <- c("v1","v2","v3","v4")
  expect_equal(test_deg_weighted, real_deg)

  # Test hyperedge degree
  test_deg <- degree(hype, "hyperedge")
  real_deg <- c(1,2,2,1)
  names(real_deg) <- c("v1","v2","v3","v4")
  expect_equal(test_deg, real_deg)

  test_deg_weighted <- degree(hype_weighted, "hyperedge")
  real_deg_weighted <- c(1,3,3,2)
  names(real_deg_weighted) <- c("v1","v2","v3","v4")
  expect_equal(test_deg_weighted, real_deg_weighted)

  # Test hyperedge degree
  test_deg <- degree(hype, "hyperedge_simple")
  real_deg <- c(1,2,2,1)
  names(real_deg) <- c("v1","v2","v3","v4")
  expect_equal(test_deg, real_deg)

  test_deg_weighted <- degree(hype_weighted, "hyperedge_simple")
  real_deg_weighted <- c(1,2,2,1)
  names(real_deg_weighted) <- c("v1","v2","v3","v4")
  expect_equal(test_deg_weighted, real_deg_weighted)

})

# Testing Cardinality -----------------------------------------------------

test_that("cardinality function works on unoriented hypergraphs", {
  hype <- example_hype()
  test_card <- cardinality(hype)
  real_card <- c(3,3)
  names(real_card) <- c("h1", "h2")
  expect_equal(test_card, real_card)
})
