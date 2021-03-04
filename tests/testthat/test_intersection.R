

test_that("intersections work", {
  h1 <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7]))
  h2 <- hypergraph_from_edgelist(list(letters[3:4],letters[3:5],letters[4:9]))
  h3 <- hypergraph_from_edgelist(list(letters[1:4],letters[3:5],letters[4:9]))

  hi <- hypergraph.intersection(h1,h2,strict=FALSE)
  his <- hypergraph.intersection(h1,h2,strict=TRUE)
  his2 <- hypergraph.intersection(h1,h3,strict=TRUE)

  expect_equal(horder(hi),5)
  expect_equal(hsize(hi),3)
  expect_true(is.empty.hypergraph(his))
  expect_equal(horder(his2),4)
  expect_equal(hsize(his2),1)

})

