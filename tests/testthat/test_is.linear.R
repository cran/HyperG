

test_that("is.linear, is.simple work", {
  s1 <- 1:5
  s2 <- 5:7
  s3 <- c(5,9:11)
  s4 <- c(5,12:15)
  h1 <- hypergraph_from_edgelist(list(s1,s2,s3,s4))
  s5 <- c(2:4)
  h2 <- hypergraph_from_edgelist(list(s1,s2,s3,s4,s5))
  s6 <- 4:6
  h3 <- hypergraph_from_edgelist(list(s1,s2,s3,s4,s6))
  s7 <- 5
  h4 <- hypergraph_from_edgelist(list(s1,s2,s3,s4,s7))

  expect_true(is.simple(h1))
  expect_true(is.linear(h1))
  expect_false(is.simple(h2))
  expect_true(is.simple(h3))
  expect_false(is.linear(h3))
  expect_false(is.simple(h4))

  e1 <- 1:3
  e2 <- c(e1,4:5)
  e3 <- c(e1,6:9)
  e4 <- c(e1,10:15)
  e5 <- c(e1,16:20)
  h5 <- hypergraph_from_edgelist(list(e2,e3,e4))
  expect_true(is.simple(h5))
  expect_false(is.linear(h5))
  h6 <- hypergraph_from_edgelist(list(e1,e2,e3,e4))
  expect_false(is.simple(h6))
  expect_false(is.linear(h6))

  s8 <- c(11,15)
  s9 <- c(13,4)
  s10 <- c(1,6)
  s11 <- c(7,10)
  h7 <- hypergraph_from_edgelist(list(s1,s2,s3,s4,s8,s9,s10,s11))
  expect_true(is.simple(h7))
  expect_true(is.linear(h7))
})

