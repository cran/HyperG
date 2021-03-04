

test_that("epsilon_hypergraph works", {

	n <- 100
	set.seed(7723)
	z1 <- matrix(rnorm(n,-2),ncol=2)
	z2 <- matrix(rnorm(n,2),ncol=2)
	x <- rbind(z1,z2)

	h <- epsilon_hypergraph(x,epsilon=quantile(as.vector(dist(x)),0.025))
	k <- reduce.hypergraph(h)

  expect_equal(horder(h), n)
  expect_equal(hsize(h), n)
  expect_equal(horder(k), n)
  expect_equal(hsize(k), 54)

  knl <- remove.loops(k)
  expect_equal(hsize(knl), 33)
  knlni <- remove.isolates(knl)
  expect_equal(horder(knlni), 79)

  set.seed(7337)
  epsilons <- runif(nrow(x),0,.75)
  he <- epsilon_hypergraph(x,epsilon=epsilons)
  expect_true(has.loops(he))
  expect_equal(max(edge_orders(he)),14)
  hr <- reduce.hypergraph(remove.loops(he))
  expect_equal(max(edge_orders(hr)),14)
  expect_equal(horder(hr),82)
  expect_equal(hsize(hr),25)

})
