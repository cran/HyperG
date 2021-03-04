

test_that("hypergraph.add.edges works", {
  h <- hypergraph_from_edgelist(list(letters[1:4],letters[3:7]))
  h1 <- hypergraph.add.edges(h, 
            list(letters[c(1,3,5)],letters[1:2],letters[4:7]))
  h2 <- hypergraph.add.edges(h, list(letters[c(1,3,5,7,9)],letters[4:8]))
  expect_equal(horder(h1), horder(h))
  expect_equal(horder(h2), horder(h)+2)

  expect_equal(hsize(h1), hsize(h) + 3)
  expect_equal(hsize(h2), hsize(h) + 2)

  expect_equal(hnames(h1), hnames(h))
  expect_equal(sort(hnames(h2)), c(hnames(h),letters[8:9]))
  h <- hypergraph_from_edgelist(list(1,1:2,c(1:2,4),c(2:3,5),3:5))
  k1 <- hypergraph.add.edges(h,list(8:9))
  expect_equal(hsize(k1),hsize(h)+1)
  k2 <- hypergraph.add.edges(h,NULL)
  expect_equal(hsize(k2),hsize(h)+1)

  h3 <- add.hyperedges(h)
  h4 <- add.hyperedges(h,NULL)
  expect_equal(h3,h4)
  expect_equal(h,remove.empty.hyperedges(h3))



})

