

test_that("hypergraph.delete.vertices works", {
  h <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7]))
  h1 <- hypergraph.delete.vertices(h, "a")
  h2 <- hypergraph.delete.vertices(h, 1)
  expect_equal(horder(h1), horder(h) - 1)
  expect_equal(horder(h2), horder(h) - 1)
  expect_equal(hnames(h1),setdiff(hnames(h),"a"))
  expect_equal(hnames(h2),setdiff(hnames(h),"a"))

  expect_equal(hyper_edges(h1), hyper_edges(h2))
})

