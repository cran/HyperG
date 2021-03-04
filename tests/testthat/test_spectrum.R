

test_that("hypergraph.spectrum works", {
  h <- hypergraph_from_edgelist(list(1:2,2:3,3:4))
  g <- graph_from_literal(1-2-3-4)
  set.seed(222)
  s <- hypergraph.spectrum(h,k=2)
  Inc <- incidence_matrix(g)
  set.seed(222)
  si <- RSpectra::svds(Inc,k=2)

  expect_equal(s, si)

  ## check that it works on small hypergraphs
  h <- hypergraph_from_edgelist(list(1:2,2:3))
  s <- hypergraph.spectrum(h)
  s1 <- svd(rbind(c(1,1,0),c(0,1,1)))
  expect_equal(s, s1)

})
