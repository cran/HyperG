

test_that("hypergraph.delete.vertices works", {
  h <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7]))
  h1 <- hypergraph.delete.edges(h, 1)
  h2 <- hypergraph_from_edgelist(list(letters[3:7]))
  h3 <- hypergraph.delete.edges(h, 1:2)
  expect_equal(hsize(h), 2)
  expect_equal(hyper_edges(h1), hyper_edges(h2))
  expect_equal(horder(h1), horder(h))
  expect_equal(horder(h2), horder(h)-2)
  expect_equal(hsize(h1), 1)
  expect_equal(hsize(h2), 1)
  expect_equal(hsize(h3), 0)

})

