test_that("eigenvector centrality works", {
  hype <- example_hype()
  test_cent <- eigenvector_centrality(hype)
  real_cent <- c(0.37174803, 0.60150096, 0.60150096, 0.37174803)
  names(real_cent) <- paste0("v", 1:4)
  expect_equal(
    test_cent,
    real_cent
  )
})

test_that("eigenvector cetrality factor works", {
  hype <- example_hype()
  test_val <- eigenvector_centrality_factor(hype)
  real_val <- 3.236068
  expect_equal(test_val, real_val)
})
