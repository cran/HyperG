

test_that("hypergraph_from_incidence_matrix works", {
  h <- hypergraph_from_edgelist(list(1:5,3:6,5:10))

  M <- matrix(0,nrow=3,ncol=10)
  M[1,1:5] <- 1
  M[2,3:6] <- 1
  M[3,5:10] <- 1
  k <- hypergraph_from_incidence_matrix(M)
  expect_equal(h,k)

  set.seed(3452)
  g <- sample_gnp(100,.1)
  M <- incidence_matrix(g)
  h <- hypergraph_from_incidence_matrix(M)
  expect_equal(edge_orders(h),apply(M,1,sum))
  expect_equal(horder(h),gorder(g))
  expect_equal(hsize(h),gsize(g))
  A <- as.matrix(Matrix::t(M) %*% M)
  expect_equal(diag(A),degree(g))
  diag(A) <- 0
  Ag <- as.matrix(as_adjacency_matrix(g))
  expect_equal(A,Ag)


})
