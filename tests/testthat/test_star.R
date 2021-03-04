

test_that("is.star and intersection_set work", {
  s1 <- 1:5
  s2 <- 5:7
  s3 <- c(5,9:11)
  s4 <- c(5,12:15)
  h1 <- hypergraph_from_edgelist(list(s1,s2,s3,s4))
  s5 <- c(2:4)
  h2 <- hypergraph_from_edgelist(list(s1,s2,s3,s4,s5))
  s6 <- 4:6
  h3 <- hypergraph_from_edgelist(list(s1,s2,s3,s4,s6))
  s7 <- c(2,5,9)
  h4 <- hypergraph_from_edgelist(list(s1,s2,s3,s4,s7))

  expect_true(is.star(h1))
  expect_true(is.star(h1,type="strong"))
  expect_false(is.star(h2))
  expect_true(is.star(h3))
  expect_false(is.star(h3,type="strong"))
  expect_true(is.star(h4))
  expect_false(is.star(h4,type="strong"))
  expect_equal(length(intersection_set(h4)),1)

  e1 <- 1:5
  e2 <- 1:3
  e3 <- c(e2,5:10)
  e4 <- c(e2,10:12)
  e5 <- c(e2,11:15)
  h5 <- hypergraph_from_edgelist(list(e1,e2,e3,e4,e5))
  expect_true(is.star(h5))
  expect_false(is.star(h5,type="strong"))
  expect_equal(length(intersection_set(h5)),length(e2))
  expect_equal(intersection_set(h5),as.character(e2))

})

