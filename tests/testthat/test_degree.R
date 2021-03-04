

test_that("hdegree works", {
  a <- letters[1:4]
  b <- letters[3:7]
  h <- hypergraph_from_edgelist(list(a,b))
  expect_equal(as.numeric(hdegree(h)),as.numeric(table(c(a,b))))
  ## make sure that degree isn't broken for graphs
  g <- as.graph(h)
  d1 <- degree(g)

  h1 <- as.hypergraph(g,method='incidence')
  expect_equal(hdegree(h1),d1)

  h2 <- as.hypergraph(g,method='adjacency')
  expect_equal(hdegree(h2),d1)
})

