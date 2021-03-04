

test_that("reduce.hypergraph works", {
  h1 <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7]))
  h2 <- hypergraph_from_edgelist(list(letters[1:4],letters[1:2],letters[1:4],letters[3:4],letters[3:7]))
  h3 <- reduce.hypergraph(h2,method='inclusion')

  expect_equal(h1,h3)

  h4 <- hypergraph.complement(h2)
  h5 <- reduce.hypergraph(h4,method='inclusion')

  h6 <- hypergraph_from_edgelist(list(letters[3:7],
                                      c(letters[1:2],letters[5:7])))
  expect_equal(h5,h6)

  h7 <- reduce.hypergraph(h2,method='intersection')
  h8 <- hypergraph_from_edgelist(list(letters[1:2],letters[1:4],letters[3:4]))
  expect_equal(h7,h8)

  h9 <- reduce.hypergraph(h2,method='union')
  h10 <- hypergraph_from_edgelist(list(letters[1:2],letters[3:7]))
  h10$M <- h10$M[2:1,]
  expect_equal(h9,h10)

})

