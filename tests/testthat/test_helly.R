

test_that("has.helly works", {
	h1 <- hypergraph_from_edgelist(list(1:5,c(2,4,6,7),c(4:6,8,9),
												  9:10))
  expect_true(has.helly(h1))
  expect_false(has.helly(h1,strong=TRUE))

  h <- hypergraph.delete.vertices(h1,4)

  expect_false(has.helly(h))

})

