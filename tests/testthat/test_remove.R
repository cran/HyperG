

test_that("removing loops, isolates and empty hyperedges works", {
  h1 <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7],letters[4]))
  h2 <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7]))
  h3 <- remove.loops(h1)

  expect_equal(h2,h3)

  h4 <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7],letters[8]))
  h5 <- hypergraph.delete.edges(h4,3)
  h6 <- remove.isolates(h5)

  expect_equal(h3,h6)

})

