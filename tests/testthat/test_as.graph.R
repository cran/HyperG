

test_that("as.graph works", {
  h <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7]))
  g <- as.graph(h)
  expect_equal(gorder(g), horder(h))
  expect_equal(gsize(g), 15)

})

