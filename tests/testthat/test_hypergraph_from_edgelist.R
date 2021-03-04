

test_that("hypergraph_from_edgelist works", {
  h <- hypergraph_from_edgelist(list(1:5,3:6,6:10))

  expect_equal(hsize(h), 3)
  expect_equal(horder(h), 10)
  M <- matrix(0,nrow=3,ncol=10)
  M[1,1:5] <- 1
  M[2,3:6] <- 1
  M[3,6:10] <- 1
  k <- hypergraph_from_incidence_matrix(M)
  expect_equal(h,k)

})
