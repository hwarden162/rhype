test_that("validate hypergraph works on basic hypergraph", {
  # Create test basic hypergraph --------------------------------------------
  hype <- example_hype()
  expect_true(validate_hypergraph(hype, verbose = FALSE, return = TRUE))

  # Test numv ---------------------------------------------------------------
  hype$set_numv(2)
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype$set_numv(NULL)
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype$set_numv("hello")
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype <- example_hype()

  # Test elist --------------------------------------------------------------
  hype$set_elist(NULL)
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype$set_elist(c(1, 2, 3))
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype <- example_hype()

  # Test vnames -------------------------------------------------------------
  hype$set_vnames(NULL)
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype$set_vnames(TRUE)
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype$set_vnames(paste0("v", 1:5))
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype$set_vnames(1:5)
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype <- example_hype()

  # Test enames -------------------------------------------------------------
  hype$set_enames(NULL)
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype$set_enames("hello")
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype$set_enames(paste("h", 1:10))
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype$set_enames(1:2)
  expect_false(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
  hype <- example_hype()
})

test_that("validate hypergraph works on oriented hypergraphs", {
  # Create test oriented hypergraph -----------------------------------------
  hype <- example_hype(oriented = TRUE)
  expect_true(validate_hypergraph(hype, verbose = FALSE, return = TRUE))
})
