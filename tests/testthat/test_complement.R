

test_that("hypergraph.complement works", {

	h1 <- hypergraph_from_edgelist(list(1:2,2:3,c(1,3)))
	h2 <- hypergraph.complement(h1)
	h3 <- hypergraph_from_edgelist(list(3,1,2))

   expect_true(equivalent.hypergraphs(h2,h3,
		 method='binary',
	    vertex.names=TRUE))

   expect_true(equivalent.hypergraphs(h2,h3,
		 method='exact',
	    vertex.names=TRUE))

	edgelist <- list(1:3,2:6,5:9,c(1,4,8),c(2,5,7,9))
	h1 <- hypergraph_from_edgelist(edgelist)
	h2 <- hypergraph.complement(h1)
	cedges <- lapply(edgelist,function(x) setdiff(1:9,x))
	h3 <- hypergraph_from_edgelist(cedges)

   expect_true(equivalent.hypergraphs(h2,h3,method='any'))


})

