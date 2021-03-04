

test_that("sample_geom_hypergraph works", {
  set.seed(34)
  h1 <- sample_geom_hypergraph(n=10,m=3,radius=.5,method="Euclidean")

  expect_equal(horder(h1),10)
  expect_equal(hsize(h1),3)
  expect_equal(edge_orders(h1),c(6,7,3))

  X <- as.matrix(expand.grid(0:5,0:5))
  Y <- rbind(c(3,3),
				 c(0,6),
				 c(-1,0),
				 c(6,2))
  set.seed(874)
  h2 <- sample_geom_hypergraph(X=X,Y=Y)

  h3 <- sample_geom_hypergraph(X=X,Y=Y,radius=h2$radius)
  h4 <- sample_geom_hypergraph(X=X,Y=Y,radius=h2$radius)
  expect_equal(h3,h2)
  expect_equal(h3,h4)
  expect_equal(horder(h3),nrow(X))
  expect_equal(hsize(h3),nrow(Y))
 expect_equal(hsize(h3),nrow(Y))

  Y2 <- rbind(c(5,5),
				 c(0,0))
  h5 <- sample_geom_hypergraph(X=X,Y=Y2,radius=2,thresh.method='geq')
  h6 <- sample_geom_hypergraph(X=X,Y=Y2,radius=2,thresh.method='leq')
  i1 <- intersect(hyper_edges(h5)[[1]],hyper_edges(h6)[[1]])
  i2 <- intersect(hyper_edges(h5)[[2]],hyper_edges(h6)[[2]])
  expect_equal(i1,c("24","34"))
  expect_equal(i2,c("3","13"))

})

