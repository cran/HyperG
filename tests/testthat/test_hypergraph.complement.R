

test_that("hypergraph.complement works", {
  h <- hypergraph_from_edgelist(list(1:2,2:3))
  hc <- hypergraph.complement(h)
  k <- hypergraph.complement(hc)

  expect_equal(h, k)
  M <- as.matrix(h$M+hc$M)
  M1 <- matrix(1,nrow=hsize(h),ncol=horder(h))
  colnames(M1) <- colnames(M)
  expect_equal(M, M1)

  set.seed(632)
  h <- sample_gnp_hypergraph(100,p=.1)
  hc <- hypergraph.complement(h)
  k <- hypergraph.complement(hc)

  expect_equal(h, k)
  M <- as.matrix(h$M+hc$M)
  M1 <- matrix(1,nrow=hsize(h),ncol=horder(h))
  colnames(M1) <- colnames(M)
  expect_equal(M, M1)


})
