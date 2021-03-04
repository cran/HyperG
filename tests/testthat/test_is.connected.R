

test_that("is.connected works", {
  h <- hypergraph_from_edgelist(list(1:2,2:3,3:4))

  expect_true(hypergraph.is.connected(h))

  h <- hypergraph_from_edgelist(list(1:2,2:3,4:5))
  expect_false(hypergraph.is.connected(h))


})
