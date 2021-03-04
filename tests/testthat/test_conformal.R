

test_that("is.conformal works", {

# Examples from Voloshin, Figure 7.15
	h1 <- hypergraph_from_edgelist(list(1:2,2:3,c(1,3)))
   expect_false(is.conformal(h1))
	expect_false(is.helly(h1))

	h2 <- hypergraph_from_edgelist(list(1:3,c(1,2,4),2:4))
   expect_false(is.conformal(h2))
	expect_true(is.helly(h2))

	h3 <- add.hyperedges(h1,list(1:3))
	expect_true(is.conformal(h3))
	expect_false(is.helly(h3))

	h4 <- add.hyperedges(h2,list(1:4))
	expect_true(is.conformal(h4))
	expect_true(is.helly(h4))

})

