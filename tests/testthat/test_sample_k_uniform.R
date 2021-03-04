

test_that("sample_k_uniform_hypergraph works", {
  set.seed(34)
  h1 <- sample_k_uniform_hypergraph(10,4,3)


  expect_equal(horder(h1),10)
  expect_equal(hsize(h1),4)
  expect_equal(unique(edge_orders(h1)),3)

  set.seed(34)
  h2 <- sample_k_regular_hypergraph(10,4,2)
  expect_equal(unique(hdegree(h2)),2,ignore_attr=TRUE)

  set.seed(2333)
  h3 <- sample_k_regular_hypergraph(100,6,5)
  h4 <- sample_k_uniform_hypergraph(100,6,5)
  expect_equal(unique(hdegree(h3)),5)
  expect_equal(unique(edge_orders(h4)),5)
  expect_false(has.isolates(h3))
  expect_true(has.isolates(h4))

})

