

test_that("make_empty_hypergraph works", {
  h <- make_empty_hypergraph(10)

  expect_true(is.empty.hypergraph(h))
  expect_equal(hsize(h), 0)
  expect_equal(horder(h), 10)

  h <- make_empty_hypergraph(0)
  expect_true(is.empty.hypergraph(h))
  expect_equal(hsize(h), 0)
  expect_equal(horder(h), 0)

})
