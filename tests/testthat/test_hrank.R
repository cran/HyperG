

test_that("hrank works", {
  s1 <- 1:4
  s2 <- 4:7
  s3 <- c(5,6,8)
  s4 <- c(1,4,7)
  h1 <- hypergraph_from_edgelist(list(s1,s2,s3,s4))
  h2 <- hypergraph_from_edgelist(list(s1,s2))

  expect_equal(hrank(h1),length(s1))
  expect_equal(hcorank(h1),length(s3))
  expect_equal(hrank(h2),length(s1))
  expect_equal(hcorank(h2),length(s1))

})

