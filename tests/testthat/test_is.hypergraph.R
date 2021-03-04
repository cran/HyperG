

test_that("is.hypergraph works", {
  h <- hypergraph_from_edgelist(list(1:2,2:3,3:4))
  g <- graph_from_literal(1-2-3-4)

  expect_true(is.hypergraph(h))
  expect_false(is.hypergraph(g))
  expect_false(is.hypergraph(h$M))

  h <- hypergraph_from_edgelist(list(1:2,2:3,4:5))
  expect_true(is.hypergraph(h))


})
