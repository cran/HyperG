

test_that("sample_gnp_hypergraph works", {
  set.seed(34)
  h1 <- sample_gnp_hypergraph(10,3,.1)

  expect_equal(horder(h1),10)
  expect_equal(hsize(h1),3)

  set.seed(345)
  lambda <- 10
  m <- rpois(1,lambda)
  set.seed(345)
  h2 <- sample_gnp_hypergraph(20,lambda=lambda,p=.1)
  expect_equal(horder(h2),20)
  expect_equal(hsize(h2),m)

  set.seed(345)
  m <- rpois(1,lambda)
  h3 <- sample_gnp_hypergraph(20,m=m,p=.1)
  expect_equal(horder(h3),20)
  expect_equal(hsize(h3),m)

  expect_equal(h2,h3)


})

