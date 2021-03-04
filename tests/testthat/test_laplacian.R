

test_that("hypergraph_laplacian_matrix works", {
  h <- hypergraph_from_edgelist(list(1:2,2:3,3:4))
  L <- hypergraph_laplacian_matrix(h)
  g <- graph_from_literal(1-2,2-3,3-4)
  L2 <- laplacian_matrix(g,sparse=FALSE)

  expect_equal(as.matrix(L),L2,ignore_attr=TRUE)

  h <- hypergraph_from_literal(a-b-c-d,a-c-d,b-d-e,a-c-e)
  L <- as.matrix(hypergraph_laplacian_matrix(h))
  dimnames(L) <- NULL

  d <- Matrix::rowSums(hypergraph_as_adjacency_matrix(h))

  expect_equal(diag(L), d,ignore_attr=TRUE)
  
  set.seed(5634)
  g <- sample_gnp(100,.1)
  h <- as.hypergraph(g,method="incidence")
  Lg <- laplacian_matrix(g)
  Lh <- hypergraph_laplacian_matrix(h)
  expect_equal(gsize(g),hsize(h))
  expect_equal(gorder(g),horder(h))
  expect_equal(as.matrix(Lg),as.matrix(Lh),ignore_attr=TRUE)

})
