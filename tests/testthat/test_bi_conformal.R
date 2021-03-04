

test_that("is.bi.conformal works", {

	## Voloshin Figure 7.15
	h <- hypergraph_from_edgelist(list(1:3,2:4,c(1,2,4),1:4))
	k <- dual_hypergraph(h)


	expect_true(is.helly(h))
	expect_true(is.helly(k))
	expect_true(is.conformal(h))
	expect_true(is.conformal(k))
	expect_true(is.bi.conformal(h))

})

