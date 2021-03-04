

test_that("hypergraph_as_adjacency_matrix works", {
  h <- hypergraph_from_edgelist(list(1:4,2:5))
  M <- hypergraph_as_adjacency_matrix(h)
  M2 <- hadjacency(h)
  expect_equal(M,M2)

  M1 <- as.matrix(M)
  ME <- matrix(0,nrow=5,ncol=5,dimnames=dimnames(M))
  ME[1,2:4] <- 1
  ME[2,c(1,5)] <- 1
  ME[2,c(3,4)] <- 2
  ME[3,c(1,5)] <- 1
  ME[3,c(2,4)] <- 2
  ME[4,c(1,5)] <- 1
  ME[4,c(2,3)] <- 2
  ME[5,2:4] <- 1

  expect_equal(M1,ME,ignore_attr=TRUE)

  h <- hypergraph_from_edgelist(list(1:2,2:3,3:4))
  g <- graph_from_literal(1-2-3-4)
  M <- as.matrix(hypergraph_as_adjacency_matrix(h))
  A <- as_adjacency_matrix(g,sparse=FALSE)
  expect_equal(M,A)

})
