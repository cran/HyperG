

test_that("graph2hypergraph works", {
  
  g1 <- graph_from_literal(1-2-3-1,2-4-6,1-5)

  h1 <- graph2hypergraph(g1,method='incidence')
  g2 <- as.graph(h1)

  expect_equal(as_adjacency_matrix(g1),as_adjacency_matrix(g2))

  h2 <- as.hypergraph(g1,method='adjacency')

  expect_equal(as_adjacency_matrix(g1),incidence_matrix(h2))

  g3 <- make_graph('Chvatal')
  h3 <- as.hypergraph(g3,method='ego')
  expect_equal(degree(g3)+1,as.numeric(edge_orders(h3)))
  h4 <- as.hypergraph(g3,method='ego',mindist=1)
  expect_equal(degree(g3),as.numeric(edge_orders(h4)))

  set.seed(3453)
  P <- rbind(c(0.2,0.1,0.0),c(0.1,0.2,0.1),c(0.0,0.1,0.3))
  g4 <- sample_sbm(150,P,rep(50,3))
  set.seed(3453)
  hs <- as.hypergraph(g4,method='spectral',fuzzy=TRUE,thresh=.1,d=2)
  expect_equal(hsize(hs),3)
  edges <- hyper_edges(hs)
  expect_equal(length(intersect(edges[[1]],edges[[2]])),2)
  expect_equal(length(intersect(edges[[3]],edges[[2]])),2)
  expect_equal(length(intersect(edges[[1]],edges[[3]])),0)
})
