

test_that("dual_hypergraph works", {

	h <- hypergraph_from_edgelist(list(1,1:2,c(1:2,4),c(2:3,5),3:5))
	h <- add.hyperedges(h)
	d <- dual_hypergraph(h)
	k <- dual_hypergraph(d)
	M <- as.matrix(incidence_matrix(h))
	N <- as.matrix(incidence_matrix(k))
	expect_equal(sum(abs(N-M)),0)
	



})

