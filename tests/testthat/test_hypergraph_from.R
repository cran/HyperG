

test_that("hypergraph_from functions work", {

  he <- hypergraph_from_edgelist(list(1:2,2:3,3:4))
  hm <- hypergraph_from_membership(c(1,1,2,2,3,3))

  expect_true(is.hypergraph(he))
  expect_equal(horder(he), 4)
  expect_equal(hsize(he), 3)
  expect_true(is.hypergraph(hm))
  expect_equal(horder(hm), 6)
  expect_equal(hsize(hm), 3)

  P <- rbind(c(0.1,0.2,0.7),
             c(0.4,0.5,0.1),
             c(0.4,0.2,0.4),
             c(0.4,0.4,0.1),
             c(0.1,0.4,0.5))
 hp <- hypergraph_from_fuzzy_clustering(P,threshold=.3)

 expect_true(is.hypergraph(hp))
 M <- as.matrix(hp$M)
 colnames(M) <- NULL
 MP <- matrix(as.numeric(P>.3),ncol=ncol(P))
 expect_equal(sum(abs(M-MP)),0)

 set.seed(143772)
 g <- sample_sbm(100,rbind(c(.15,.1),c(.1,.15)),c(50,50))
 m <- mclust::Mclust(ase(g),G=2,verbose=FALSE)
 hs <- hypergraph_from_spectral_clustering(g,m=m)
 hsf <- hypergraph_from_spectral_clustering(g,m=m,fuzzy=TRUE,thresh=.25)
 expect_false(hypergraph.is.connected(hs))
 expect_true(hypergraph.is.connected(hsf))
 M <- as.matrix(hsf$M)
 colnames(M) <- NULL
 P <- t(m$z)
 MP <- matrix(as.numeric(P>.25),ncol=ncol(P))
 expect_equal(sum(abs(M-MP)),0)

 I <- hm$M
 hm2 <- hypergraph_from_incidence_matrix(I)
 expect_equal(sum(abs(hm2$M-I)),0)


hl <- hypergraph_from_literal(a-b-c,c-d-e,f-g-a-b,f-h-c-e)
hel <- hypergraph_from_edgelist(list(c("a","b","c"),
                                     c("c","d","e"),
                                     c("f","g","a","b"),
												 c("f","h","c","e")))

 expect_equal(hl,hel)

})
