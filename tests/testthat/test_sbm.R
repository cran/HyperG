

test_that("stochastic block model works", {
  P <- matrix(0.05,ncol=4,nrow=4)
  diag(P) <- 0.2
  ns <- rep(50,4)
  set.seed(5429)
  h <- sample_sbm_hypergraph(P=P,block.sizes=ns,d=4)

  expect_true(all(edge_orders(h)==4))

  edges <- hypergraph_as_edgelist(h)
  cls <- rep(1:4,each=50)
  purity <- unlist(lapply(edges,function(x) {
     y <- as.numeric(x)
	  length(unique(cls[y]))
     }))
  p <- table(purity)
  expect_equal(length(p),2)
  expect_equal(max(purity),2)

  set.seed(5429)
  h <- sample_sbm_hypergraph(P=P,block.sizes=ns,d=4,variable.size=TRUE,
            impurity=2)

  expect_true(round(mean(edge_orders(h)),0)==6)

  edges <- hypergraph_as_edgelist(h)
  cls <- rep(1:4,each=50)
  purity <- unlist(lapply(edges,function(x) {
     y <- as.numeric(x)
	  length(unique(cls[y]))
     }))
  p <- table(purity)
  expect_equal(length(p),4)
  expect_equal(max(purity),4)

  set.seed(5429)
  h <- sample_sbm_hypergraph(P=P,block.sizes=ns,d=4,variable.size=FALSE,
            impurity=2)

  edges <- hypergraph_as_edgelist(h)
  cls <- rep(1:4,each=50)
  purity <- unlist(lapply(edges,function(x) {
     y <- as.numeric(x)
	  length(unique(cls[y]))
     }))
  expect_true(all(purity>1))

})
