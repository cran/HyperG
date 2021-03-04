

test_that("line.graph works", {
  h1 <- hypergraph_from_edgelist(list(1:3,
                                      2:5,
												  4:8,
												  8:10,
												  c(1,9:10)))
  g1 <- line.graph(h1)
  r5 <- make_ring(5)
  expect_true(isomorphic(g1,r5))

  p <- make_graph("petersen")

  h2 <- as.hypergraph(p,method='adjacency')
  g2 <- line.graph(h2)
  expect_true(all(degree(g2)==6))

  h3 <- as.hypergraph(r5,method='adjacency')
  g3 <- line.graph(h3)
  expect_true(isomorphic(r5,g3))

  r11 <- make_ring(11)
  h4 <- as.hypergraph(r11,method='ego')
  g4 <- line.graph(h4)
  g5 <- add_edges(r11,c(1,3,2,4,3,5,4,6,5,7,6,8,7,9,8,10,9,11,10,1,11,2))
  expect_true(isomorphic(g4,g5))

})

