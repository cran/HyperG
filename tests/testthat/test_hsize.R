

test_that("hsize works", {
  e1 <- list(letters[1:4],letters[3:7])
  h1 <- hypergraph_from_edgelist(e1)
  e2 <- list(letters[1:4],letters[1:2],
				  letters[1:4],letters[3:4],letters[3:7])
  h2 <- hypergraph_from_edgelist(e2)

  expect_equal(hsize(h1),length(e1))
  expect_equal(hsize(h2),length(e2))

})

