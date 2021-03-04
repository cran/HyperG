

test_that("incidence_matrix works", {
  g <- graph_from_literal(a-b,b-c-a,a-d-e-f-b)
  M1 <- incidence_matrix(g)
  el <- as_edgelist(g)
  M2 <- Matrix::Matrix(0,nrow=nrow(el),ncol=ncol(M1),
      dimnames=list(NULL,colnames(M1)))
  for(i in 1:nrow(el)) M2[i,el[i,]] <- 1
  expect_equal(M1, M2)
  h <- hypergraph_from_edgelist(list(1:3,4:5,2:4,5:7))
  I1 <- incidence_matrix(h)
  I2 <- hypergraph_as_incidence_matrix(h)
  expect_equal(I1,I2)

})

