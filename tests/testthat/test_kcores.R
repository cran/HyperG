

test_that("kcores works", {
  edges <- list(c("A", "C"), 
        c("B", "C"), 
		  c("C", "E"), 
		  c("C", "F"), 
		  c("E", "D"), 
		  c("E", "F"), 
		  c("D", "G"), 
		  c("D", "H"), 
		  c("D", "J"), 
		  c("H", "G"), 
		  c("H", "J"), 
		  c("G", "J"), 
		  c("J", "M"), 
		  c("J", "K"), 
		  c("M", "K"), 
		  c("M", "O"), 
		  c("M", "N"), 
		  c("K", "N"), 
		  c("K", "F"), 
		  c("K", "I"), 
		  c("K", "L"), 
		  c("F", "I"), 
		  c("I", "L"), 
		  c("F", "L"), 
		  c("P", "Q"), 
		  c("Q", "R"), 
		  c("Q", "S"), 
		  c("R", "T"), 
		  c("S", "T"))
	h <- hypergraph_from_edgelist(edges,v=union(unlist(edges),"U"))
	ans <- c(1,1,2,3,2,3,3,3,3,3,3,3,2,2,1,1,2,2,2,2,0)
	kc <- kCores(h)


  expect_equal(kc[order(names(kc))], ans,ignore_attr=TRUE)

})
