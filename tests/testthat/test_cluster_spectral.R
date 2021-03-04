

test_that("cluster_spectral works", {
  set.seed(3344)
  g <- sample_sbm(200,rbind(c(.1,.025),c(.025,.2)),c(100,100))
  set.seed(362)
  z1 <- ase(g,d=3)
  m1 <- mclust::Mclust(z1,verbose=FALSE)
  expect_equal(m1$G, 2)
  expect_equal(sum(m1$classification==rep(1:2,each=100)), 200)

})

