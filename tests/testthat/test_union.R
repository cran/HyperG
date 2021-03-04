

test_that("unions work", {
  h1 <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7]))
  h2 <- hypergraph_from_edgelist(list(letters[3:4],letters[3:5],letters[4:9]))

  hd <- hypergraph.disjoint.union(h1,h2)
  hu <- hypergraph.union(h1,h2,reduce=FALSE)
  hur <- hypergraph.union(h1,h2,reduce=TRUE)

  expect_equal(horder(hd), horder(h1)+horder(h2))
  expect_equal(hsize(hd), hsize(h1)+hsize(h2))
  expect_equal(hsize(hu), hsize(h1)+hsize(h2))
  expect_equal(hsize(hur), 3)
  expect_equal(horder(hu), 9)
  expect_equal(horder(hur), 9)
})

