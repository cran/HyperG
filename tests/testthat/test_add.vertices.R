

test_that("hypergraph.add.vertices works", {
  h <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7]))
  h1 <- hypergraph.add.vertices(h, (nv <- 2))
  expect_equal(horder(h1), horder(h) + nv)
  h2 <- hypergraph.add.vertices(h, (nv <- 2),(nms <- letters[horder(h)+(1:nv)]))
  expect_equal(horder(h2), horder(h) + nv)
  expect_equal(hnames(h2), c(hnames(h),letters[horder(h)+(1:nv)]))
  expect_equal(hsize(h1), hsize(h))
  expect_equal(hsize(h2), hsize(h))
  expect_equal(hyper_edges(h1), hyper_edges(h2))
  expect_equal(hyper_edges(h1), hyper_edges(h))
})

