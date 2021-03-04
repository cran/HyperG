

test_that("as.edgelist works", {
  h <- hypergraph_from_edgelist(list(1:5,3:6,6:10))
  e1 <- hypergraph_as_edgelist(h)
  e2 <- as_edgelist(as.graph(h))

  expect_equal(length(e1), 3)
  expect_equal(nrow(e2), 23)

  h <- hypergraph_from_edgelist(list(1:5))
  e1 <- hypergraph_as_edgelist(h)
  expect_equal(e1,list(as.character(1:5)))

  g1 <- as.graph(h)
  g2 <- make_full_graph(5)
  expect_true(isomorphic(g1,g2))

  set.seed(100)
  g1 <- sample_sbm(100,rbind(c(.1,.05),c(.05,.1)),c(50,50))
  h <- as.hypergraph(incidence_matrix(g1))
  x <- do.call(rbind,lapply(hypergraph_as_edgelist(h),as.numeric))
  y <- as_edgelist(g1)
  expect_equal(x,y)

})
