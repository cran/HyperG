

test_that("hypergraph.entropy works", {
  e1 <- list(letters[1:3],letters[3:5])
  h1 <- hypergraph_from_edgelist(e1)
  l1 <- hypergraph.entropy(h1)

  expect_equal(l1,1.825011,tolerance=1E-6)

  e2 <- list(letters[1:4],letters[1:2],
				  letters[1:4],letters[3:4],letters[3:7])
  h2 <- hypergraph_from_edgelist(e2)
  l2 <- hypergraph.entropy(h2)

  expect_equal(l2,2.386676,tolerance=1E-6)

})

